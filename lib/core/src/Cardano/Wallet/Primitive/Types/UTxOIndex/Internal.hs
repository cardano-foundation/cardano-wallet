{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Provides internal functions for the 'UTxOIndex' type, which indexes a UTxO
-- set by asset identifier.
--
-- The index makes it possible to efficiently compute the subset of a UTxO set
-- containing a particular asset, or to select just a single UTxO containing a
-- particular asset, without having to search linearly through the entire UTxO
-- set.
--
-- See the documentation for 'UTxOIndex' for more details.
--
module Cardano.Wallet.Primitive.Types.UTxOIndex.Internal
    (
    ----------------------------------------------------------------------------
    -- Public Interface
    ----------------------------------------------------------------------------

    -- * Type

      -- Important:
      --
      -- The default data constructor for 'UTxOIndex' is not exported, by
      -- design, as the internal data structure has an invariant that must
      -- be preserved across all operations.
      --
      -- See the 'checkInvariant' function for more details.
      --
      UTxOIndex

    -- * Construction
    , empty
    , singleton
    , fromSequence
    , fromUTxO

    -- * Deconstruction
    , toList
    , toUTxO

    -- * Folding
    , fold

    -- * Modification
    , insert
    , insertMany
    , delete
    , deleteMany

    -- * Filtering and partitioning
    , filter
    , partition

    -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

    -- * Set operations
    , difference
    , disjoint

    -- * Selection
    , SelectionFilter (..)
    , selectRandom
    , selectRandomWithPriority

    ----------------------------------------------------------------------------
    -- Internal Interface
    ----------------------------------------------------------------------------

    -- * Utilities
    , selectRandomSetMember

    -- * Invariant
    , InvariantStatus (..)
    , checkInvariant

    ) where

import Prelude hiding
    ( filter, lookup, null )

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Control.DeepSeq
    ( NFData )
import Control.Monad.Extra
    ( firstJustM )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Bifunctor
    ( bimap )
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
    ( isJust )
import Data.Set
    ( Set )
import Data.Set.Strict.NonEmptySet
    ( NonEmptySet )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.Strict.NonEmptySet as NonEmptySet

--------------------------------------------------------------------------------
-- Public Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A UTxO set that is indexed by asset identifier.
--
-- The index provides a mapping from assets to subsets of the UTxO set.
--
-- A UTxO appears in the set for a particular asset if and only if its
-- associated value has a non-zero quantity of that asset.
--
-- The index makes it possible to efficiently compute the subset of a UTxO set
-- containing a particular asset, or to select just a single UTxO containing a
-- particular asset, without having to search linearly through the entire UTxO
-- set.
--
-- The index also keeps track of the current UTxO balance of all assets, making
-- it possible to efficiently look up the total quantity of a particular asset
-- without having to sum across the entire UTxO set.
--
-- The UTxO index data structure has an invariant that can be checked with
-- the 'checkInvariant' function.
--
data UTxOIndex u = UTxOIndex
    { assetsAll
        :: !(Map AssetId (NonEmptySet u))
        -- An index of all entries that contain at least one non-ada asset.
    , assetsSingleton
        :: !(Map AssetId (NonEmptySet u))
        -- An index of all entries that contain exactly one non-ada asset.
    , coins
        :: !(Set u)
        -- An index of all entries that contain no non-ada assets.
    , balance
        :: !TokenBundle
        -- The total balance of all entries.
    , universe
        :: !(Map u TokenBundle)
        -- The complete set of all entries.
    }
    deriving (Eq, Generic, Read, Show)

instance NFData u => NFData (UTxOIndex u)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | An index with no entries.
--
empty :: UTxOIndex u
empty = UTxOIndex
    { assetsAll = Map.empty
    , assetsSingleton = Map.empty
    , coins = Set.empty
    , balance = TokenBundle.empty
    , universe = Map.empty
    }

-- | Creates a singleton index from the specified UTxO identifier and value.
--
singleton :: Ord u => u -> TokenBundle -> UTxOIndex u
singleton u b = insertUnsafe u b empty

-- | Constructs an index from a sequence of entries.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
-- If the given sequence contains more than one mapping for the same UTxO
-- identifier, the mapping that appears latest in the sequence will take
-- precendence, and all others will be ignored.
--
fromSequence :: (Foldable f, Ord u) => f (u, TokenBundle) -> UTxOIndex u
fromSequence = flip insertMany empty

-- | Constructs an index from a map.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
fromUTxO :: Ord u => Map u TokenBundle -> UTxOIndex u
fromUTxO = fromSequence . Map.toList

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts an index to a list of its constituent entries.
--
-- Consider using 'fold' if your goal is to consume all entries in the output.
--
toList :: UTxOIndex u -> [(u, TokenBundle)]
toList = fold (\ubs u b -> (u, b) : ubs) []

-- | Converts an index into a map.
--
-- Consider using 'fold' if your goal is to consume all entries in the output.
--
toUTxO :: UTxOIndex u -> Map u TokenBundle
toUTxO = universe

--------------------------------------------------------------------------------
-- Folding
--------------------------------------------------------------------------------

-- | Folds strictly over the constituent entries of an index.
--
fold :: (a -> u -> TokenBundle -> a) -> a -> UTxOIndex u -> a
fold f a = Map.foldlWithKey' f a . universe

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Inserts an entry that maps the given UTxO identifier to the given value.
--
-- If the index has an existing value for the specified UTxO identifier, the
-- value referred to by that identifier will be replaced with the specified
-- value.
--
insert :: Ord u => u -> TokenBundle -> UTxOIndex u -> UTxOIndex u
insert u b = insertUnsafe u b . delete u

-- | Inserts multiple entries into an index.
--
-- See 'insert'.
--
insertMany
    :: (Foldable f, Ord u)
    => f (u, TokenBundle)
    -> UTxOIndex u
    -> UTxOIndex u
insertMany = flip $ F.foldl' $ \i (u, b) -> insert u b i

-- | Deletes the entry corresponding to the given UTxO identifier.
--
-- If the index has no existing entry for the specified identifier, the result
-- of applying this function will be equivalent to the identity function.
--
delete :: forall u. Ord u => u -> UTxOIndex u -> UTxOIndex u
delete u i =
    maybe i updateIndex $ Map.lookup u $ universe i
  where
    updateIndex :: TokenBundle -> UTxOIndex u
    updateIndex b = i
        -- This operation is safe, since we have already determined that the
        -- entry is a member of the index, and therefore the balance must be
        -- greater than or equal to the value of this output:
        & over #balance (`TokenBundle.unsafeSubtract` b)
        & over #universe (Map.delete u)
        & case categorizeTokenBundle b of
            IsCoin ->
                over #coins (Set.delete u)
            IsCoinWithSingletonAsset a -> id
                . over #assetsSingleton (`deleteEntry` a)
                . over #assetsAll (`deleteEntry` a)
            IsCoinWithMultipleAssets as ->
                over #assetsAll (flip (F.foldl' deleteEntry) as)

    deleteEntry
        :: Map AssetId (NonEmptySet u)
        -> AssetId
        -> Map AssetId (NonEmptySet u)
    deleteEntry m a = Map.update (NonEmptySet.delete u) a m

-- | Deletes multiple entries from an index.
--
-- See 'delete'.
--
deleteMany :: (Foldable f, Ord u) => f u -> UTxOIndex u -> UTxOIndex u
deleteMany = flip $ F.foldl' $ \i u -> delete u i

--------------------------------------------------------------------------------
-- Filtering and partitioning
--------------------------------------------------------------------------------

-- | Filters an index.
--
filter :: Ord u => (u -> Bool) -> UTxOIndex u -> UTxOIndex u
filter f = fromSequence . L.filter (f . fst) . toList

-- | Partitions an index.
--
partition :: Ord u => (u -> Bool) -> UTxOIndex u -> (UTxOIndex u, UTxOIndex u)
partition f = bimap fromSequence fromSequence . L.partition (f . fst) . toList

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Returns the complete set of all assets contained in an index.
--
assets :: UTxOIndex u -> Set AssetId
assets = Map.keysSet . assetsAll

-- | Returns the value corresponding to the given UTxO identifier.
--
-- If the index has no such identifier, this function returns 'Nothing'.
--
lookup :: Ord u => u -> UTxOIndex u -> Maybe TokenBundle
lookup u = Map.lookup u . universe

-- | Returns 'True' if (and only if) the index has an entry for the given UTxO
--   identifier.
--
member :: Ord u => u -> UTxOIndex u -> Bool
member u = isJust . lookup u

-- | Returns 'True' if (and only if) the index is empty.
--
null :: UTxOIndex u -> Bool
null = (== 0) . size

-- | Returns the total number of UTxO entries held within the index.
--
size :: UTxOIndex u -> Int
size = Map.size . universe

--------------------------------------------------------------------------------
-- Set operations
--------------------------------------------------------------------------------

difference :: Ord u => UTxOIndex u -> UTxOIndex u -> UTxOIndex u
difference a b = fromSequence
    $ Map.toList
    $ Map.difference (universe a) (universe b)

-- | Indicates whether a pair of UTxO indices are disjoint.
--
disjoint :: Ord u => UTxOIndex u -> UTxOIndex u -> Bool
disjoint i1 i2 = universe i1 `Map.disjoint` universe i2

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

-- | Specifies a filter for selecting UTxO entries.
--
data SelectionFilter
    = Any
        -- ^ Select any UTxO entry from the entire set.
    | WithAdaOnly
        -- ^ Select any UTxO entry that only has ada and no other assets.
    | WithAsset AssetId
        -- ^ Select any UTxO entry that has a non-zero quantity of the specified
        -- asset.
    | WithAssetOnly AssetId
        -- ^ Select any UTxO entry that has a non-zero quantity of the specified
        -- asset, but no other non-ada assets.
    deriving (Eq, Show)

-- | Selects an entry at random from the index according to the given filter.
--
-- Returns the selected entry and an updated index with the entry removed.
--
-- Returns 'Nothing' if there were no matching entries.
--
selectRandom
    :: forall m u. (MonadRandom m, Ord u)
    => UTxOIndex u
    -> SelectionFilter
    -> m (Maybe ((u, TokenBundle), UTxOIndex u))
selectRandom i selectionFilter =
    (lookupAndRemoveEntry =<<) <$> selectRandomSetMember selectionSet
  where
    lookupAndRemoveEntry :: u -> Maybe ((u, TokenBundle), UTxOIndex u)
    lookupAndRemoveEntry u =
        (\b -> ((u, b), delete u i)) <$> Map.lookup u (universe i)

    selectionSet :: Set u
    selectionSet = case selectionFilter of
        Any -> Map.keysSet $ universe i
        WithAdaOnly -> entriesWithAdaOnly i
        WithAsset a -> entriesWithAsset a i
        WithAssetOnly a -> entriesWithAssetOnly a i

-- | Selects an entry at random from the index according to the given filters.
--
-- This function traverses the specified list of filters in descending order of
-- priority, from left to right.
--
-- When considering a particular filter:
--
--    - if the function is able to select a UTxO entry that matches, it
--      terminates with that entry and an updated index with the entry removed.
--
--    - if the function is not able to select a UTxO entry that matches, it
--      traverses to the next filter available.
--
-- This function returns 'Nothing' if (and only if) it traverses the entire
-- list of filters without successfully selecting a UTxO entry.
--
selectRandomWithPriority
    :: (MonadRandom m, Ord u)
    => UTxOIndex u
    -> NonEmpty SelectionFilter
    -- ^ A list of selection filters to be traversed in descending order of
    -- priority, from left to right.
    -> m (Maybe ((u, TokenBundle), UTxOIndex u))
selectRandomWithPriority i =
    firstJustM (selectRandom i) . NE.toList

--------------------------------------------------------------------------------
-- Internal Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Represents different categories of token bundles.
--
data BundleCategory
    = IsCoin
    | IsCoinWithSingletonAsset AssetId
    | IsCoinWithMultipleAssets (Set AssetId)
    deriving (Eq, Show)

-- | Categorizes a token bundle by what kind of assets it contains.
--
categorizeTokenBundle :: TokenBundle -> BundleCategory
categorizeTokenBundle b = case F.toList bundleAssets of
    []  -> IsCoin
    [a] -> IsCoinWithSingletonAsset a
    _   -> IsCoinWithMultipleAssets bundleAssets
  where
    bundleAssets = TokenBundle.getAssets b

-- | Returns the set of keys for entries that have no assets other than ada.
--
entriesWithAdaOnly :: UTxOIndex u -> Set u
entriesWithAdaOnly = coins

-- | Returns the set of keys for entries that have non-zero quantities of the
--   given asset.
--
entriesWithAsset :: Ord u => AssetId -> UTxOIndex u -> Set u
entriesWithAsset a =
    maybe mempty NonEmptySet.toSet . Map.lookup a . assetsAll

-- | Returns the set of keys for entries that have no non-ada assets other than
--   the given asset.
--
entriesWithAssetOnly :: Ord u => AssetId -> UTxOIndex u -> Set u
entriesWithAssetOnly a =
    maybe mempty NonEmptySet.toSet . Map.lookup a . assetsSingleton

-- Inserts an entry, but without checking the following pre-condition:
--
-- Pre-condition: there is no existing entry for the specified UTxO identifier.
--
-- See 'insert' for a safe version of this function.
--
insertUnsafe
    :: forall u. Ord u
    => u
    -> TokenBundle
    -> UTxOIndex u
    -> UTxOIndex u
insertUnsafe u b i = i
    & over #balance (`TokenBundle.add` b)
    & over #universe (Map.insert u b)
    & case categorizeTokenBundle b of
        IsCoin ->
            over #coins (Set.insert u)
        IsCoinWithSingletonAsset a -> id
            . over #assetsSingleton (`insertEntry` a)
            . over #assetsAll (`insertEntry` a)
        IsCoinWithMultipleAssets as ->
            over #assetsAll (flip (F.foldl' insertEntry) as)
  where
    insertEntry
        :: Map AssetId (NonEmptySet u)
        -> AssetId
        -> Map AssetId (NonEmptySet u)
    insertEntry m a =
        Map.alter (maybe (Just createNew) (Just . updateOld)) a m
      where
        createNew = NonEmptySet.singleton u
        updateOld = NonEmptySet.insert u

-- | Selects an element at random from the given set.
--
-- Returns 'Nothing' if (and only if) the given set is empty.
--
selectRandomSetMember
    :: MonadRandom m
    => Set a
    -> m (Maybe a)
selectRandomSetMember s
    | Set.null s =
        pure Nothing
    | otherwise =
        Just . flip Set.elemAt s <$> getRandomR (0, Set.size s - 1)

--------------------------------------------------------------------------------
-- Invariant
--------------------------------------------------------------------------------

-- | The result of checking the invariant with the 'checkInvariant' function.
--
data InvariantStatus
    = InvariantHolds
      -- ^ Indicates a successful check of the invariant.
    | InvariantBalanceError BalanceError
      -- ^ Indicates that the cached 'balance' value is incorrect.
    | InvariantIndexIncomplete
      -- ^ Indicates that the 'index' is missing one or more entries.
    | InvariantIndexNonMinimal
      -- ^ Indicates that the 'index' has one or more unnecessary entries.
    | InvariantAssetsInconsistent
      -- ^ Indicates that the 'index' and the cached 'balance' value disagree
      --   about which assets are included.
    deriving (Eq, Show)

-- | Checks whether or not the invariant holds.
--
checkInvariant :: Ord u => UTxOIndex u -> InvariantStatus
checkInvariant i
    | balanceStatus /= BalanceCorrect =
        InvariantBalanceError balanceError
    | not (indexIsComplete i) =
        InvariantIndexIncomplete
    | not (indexIsMinimal i) =
        InvariantIndexNonMinimal
    | not (assetsConsistent i) =
        InvariantAssetsInconsistent
    | otherwise =
        InvariantHolds
  where
    balanceStatus = checkBalance i
    BalanceIncorrect balanceError = balanceStatus

-- | Indicates whether on not the stored 'balance' value is correct.
--
data BalanceStatus
    = BalanceCorrect
    | BalanceIncorrect BalanceError
    deriving (Eq, Show)

-- | Indicates that the stored 'balance' value is not correct.
--
data BalanceError = BalanceError
    { balanceComputed
        :: TokenBundle
    , balanceStored
        :: TokenBundle
    }
    deriving (Eq, Show)

-- | Checks that calculating the balance from scratch gives a result that
--   is equal to the stored 'balance' value.
--
checkBalance :: UTxOIndex u -> BalanceStatus
checkBalance i
    | balanceComputed == balanceStored =
        BalanceCorrect
    | otherwise =
        BalanceIncorrect $ BalanceError {balanceComputed, balanceStored}
  where
    balanceComputed = F.fold (universe i)
    balanceStored = balance i

-- | Checks that every entry in the 'universe' map is properly indexed.
--
indexIsComplete :: forall u. Ord u => UTxOIndex u -> Bool
indexIsComplete i = F.all hasEntry $ Map.toList $ universe i
  where
    hasEntry :: (u, TokenBundle) -> Bool
    hasEntry (u, b) = case categorizeTokenBundle b of
        IsCoin ->
            u `Set.member` coins i
        IsCoinWithSingletonAsset a -> (&&)
            (hasEntryForAsset a u assetsAll)
            (hasEntryForAsset a u assetsSingleton)
        IsCoinWithMultipleAssets as ->
            F.all (\a -> hasEntryForAsset a u assetsAll) as

    hasEntryForAsset
        :: AssetId
        -> u
        -> (UTxOIndex u -> Map AssetId (NonEmptySet u))
        -> Bool
    hasEntryForAsset asset u assetsMap =
        maybe False (NonEmptySet.member u) $ Map.lookup asset $ assetsMap i

-- | Checks that every indexed entry is required by some entry in the 'universe'
--   map.
--
indexIsMinimal :: forall u. Ord u => UTxOIndex u -> Bool
indexIsMinimal i = F.and
    [ assetsAll i
        & Map.toList
        & F.all (\(a, u) -> F.all (entryHasAsset a) u)
    , assetsSingleton i
        & Map.toList
        & F.all (\(a, u) -> F.all (entryHasSingletonAsset a) u)
    , coins i
        & F.all entryIsCoin
    ]
  where
    entryHasAsset :: AssetId -> u -> Bool
    entryHasAsset a = entryMatches (`TokenBundle.hasQuantity` a)

    entryHasSingletonAsset :: AssetId -> u -> Bool
    entryHasSingletonAsset a = entryMatches $
        (== IsCoinWithSingletonAsset a) . categorizeTokenBundle

    entryIsCoin :: u -> Bool
    entryIsCoin = entryMatches $
        (== IsCoin) . categorizeTokenBundle

    entryMatches :: (TokenBundle -> Bool) -> u -> Bool
    entryMatches test u = maybe False test $ Map.lookup u $ universe i

-- | Checks that the asset sets are consistent.
--
-- In particular, the set of assets in the cached 'balance' must be:
--
--    - equal to the set of assets in 'assetsAll'
--    - a superset of the set of assets in 'assetsSingleton'.
--
assetsConsistent :: UTxOIndex u -> Bool
assetsConsistent i = (&&)
    (Map.keysSet (assetsAll i) == balanceAssets)
    (Map.keysSet (assetsSingleton i) `Set.isSubsetOf` balanceAssets)
  where
    balanceAssets = TokenBundle.getAssets (balance i)
