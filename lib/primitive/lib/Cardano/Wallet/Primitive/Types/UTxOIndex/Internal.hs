{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{- HLINT ignore "Use &&" -}

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
    , fromMap

    -- * Deconstruction
    , toList
    , toMap

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

    -- * Assets
    , Asset (..)
    , tokenBundleAssets
    , tokenBundleAssetCount
    , tokenBundleHasAsset

    -- * Token bundle categorization
    , BundleCategory (..)
    , categorizeTokenBundle

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
import Data.MonoidMap
    ( MonoidMap )
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.MonoidMap as MonoidMap
import qualified Data.Set as Set

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
    { indexAll
        :: !(MonoidMap Asset (Set u))
        -- An index of all entries that contain the given asset.
    , indexSingletons
        :: !(MonoidMap Asset (Set u))
        -- An index of all entries that contain the given asset and no other
        -- assets.
    , indexPairs
        :: !(MonoidMap Asset (Set u))
        -- An index of all entries that contain the given asset and exactly
        -- one other asset.
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
    { indexAll = MonoidMap.empty
    , indexSingletons = MonoidMap.empty
    , indexPairs = MonoidMap.empty
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
-- precedence, and all others will be ignored.
--
fromSequence :: (Foldable f, Ord u) => f (u, TokenBundle) -> UTxOIndex u
fromSequence = flip insertMany empty

-- | Constructs an index from a map.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
-- Satisfies the following property:
--
-- @
-- 'fromMap' ≡ 'fromSequence' . 'Map.toList'
-- @
--
fromMap :: Ord u => Map u TokenBundle -> UTxOIndex u
fromMap = Map.foldlWithKey' (\i u b -> insertUnsafe u b i) empty

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
toMap :: UTxOIndex u -> Map u TokenBundle
toMap = universe

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
        & over #balance (`TokenBundle.difference` b)
        & over #universe (Map.delete u)
        & case categorizeTokenBundle b of
            BundleWithNoAssets -> id
            BundleWithOneAsset a -> id
                . over #indexAll (`deleteEntry` a)
                . over #indexSingletons (`deleteEntry` a)
            BundleWithTwoAssets (a1, a2) -> id
                . over #indexAll (`deleteEntry` a1)
                . over #indexAll (`deleteEntry` a2)
                . over #indexPairs (`deleteEntry` a1)
                . over #indexPairs (`deleteEntry` a2)
            BundleWithMultipleAssets as -> id
                . over #indexAll (flip (F.foldl' deleteEntry) as)

    deleteEntry
        :: Ord asset
        => MonoidMap asset (Set u)
        -> asset
        -> MonoidMap asset (Set u)
    deleteEntry m a = MonoidMap.adjust (Set.delete u) a m

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
assets :: UTxOIndex u -> Set Asset
assets = MonoidMap.nonNullKeys . indexAll

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

-- | Creates a new index by subtracting the second index from the first.
--
-- This operation is fast if the intersection of the first and second indices
-- is small relative to the size of the first index.
--
difference :: Ord u => UTxOIndex u -> UTxOIndex u -> UTxOIndex u
difference a b
    | disjoint a b = a
    | otherwise = deleteMany keysToDelete a
  where
    keysToDelete =
        Set.intersection
            (Map.keysSet (universe a))
            (Map.keysSet (universe b))

-- | Indicates whether a pair of UTxO indices are disjoint.
--
disjoint :: Ord u => UTxOIndex u -> UTxOIndex u -> Bool
disjoint i1 i2 = universe i1 `Map.disjoint` universe i2

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

-- | Specifies a filter for selecting UTxO entries.
--
data SelectionFilter asset
    = SelectSingleton asset
      -- ^ Matches UTxOs that contain only the given asset and no other assets.
    | SelectPairWith asset
      -- ^ Matches UTxOs that contain the given asset and exactly one other
      -- asset.
    | SelectAnyWith asset
      -- ^ Matches UTxOs that contain the given asset and any number of other
      -- assets.
    | SelectAny
      -- ^ Matches all UTxOs regardless of what assets they contain.
    deriving (Eq, Foldable, Functor, Show, Traversable)

-- | Selects an entry at random from the index according to the given filter.
--
-- Returns the selected entry and an updated index with the entry removed.
--
-- Returns 'Nothing' if there were no matching entries.
--
selectRandom
    :: forall m u. (MonadRandom m, Ord u)
    => UTxOIndex u
    -> SelectionFilter Asset
    -> m (Maybe ((u, TokenBundle), UTxOIndex u))
selectRandom i selectionFilter =
    (lookupAndRemoveEntry =<<) <$> selectRandomSetMember selectionSet
  where
    lookupAndRemoveEntry :: u -> Maybe ((u, TokenBundle), UTxOIndex u)
    lookupAndRemoveEntry u =
        (\b -> ((u, b), delete u i)) <$> Map.lookup u (universe i)

    selectionSet :: Set u
    selectionSet = case selectionFilter of
        SelectSingleton a ->
            a `lookupWith` indexSingletons
        SelectPairWith a ->
            a `lookupWith` indexPairs
        SelectAnyWith a ->
            a `lookupWith` indexAll
        SelectAny ->
            Map.keysSet (universe i)
      where
        a `lookupWith` index = MonoidMap.get a $ index i

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
    -> NonEmpty (SelectionFilter Asset)
    -- ^ A list of selection filters to be traversed in descending order of
    -- priority, from left to right.
    -> m (Maybe ((u, TokenBundle), UTxOIndex u))
selectRandomWithPriority i =
    firstJustM (selectRandom i) . NE.toList

--------------------------------------------------------------------------------
-- Internal Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Assets
--------------------------------------------------------------------------------

-- | A type capable of representing any asset, including both ada and non-ada
--   assets.
--
-- TODO: ADP-1449
-- Move this type away from the 'UTxOIndex' module and replace all usages of it
-- with a type parameter.
--
data Asset
    = AssetLovelace
    | Asset AssetId
    deriving (Eq, Generic, Ord, Read, Show)

deriving instance NFData Asset

-- | Returns the set of assets associated with a given 'TokenBundle'.
--
-- Both ada and non-ada assets are included in the set returned.
--
-- TODO: ADP-1449
-- Move this function away from the 'UTxOIndex' module once the type of assets
-- has been generalized.
--
tokenBundleAssets :: TokenBundle -> Set Asset
tokenBundleAssets b = Set.union
    (Set.fromList [AssetLovelace | TokenBundle.coin b /= mempty])
    (Set.map Asset (TokenBundle.getAssets b))

-- | Returns the number of assets associated with a given 'TokenBundle'.
--
-- Both ada and non-ada assets are included in the total count returned.
--
-- TODO: ADP-1449
-- Move this function away from the 'UTxOIndex' module once the type of assets
-- has been generalized.
--
tokenBundleAssetCount :: TokenBundle -> Int
tokenBundleAssetCount b = (+)
    (if TokenBundle.coin b /= mempty then 1 else 0)
    (TokenMap.size (TokenBundle.tokens b))

-- | Indicates whether or not a given bundle includes a given asset.
--
-- Both ada and non-ada assets can be queried.
--
-- TODO: ADP-1449
-- Move this function away from the 'UTxOIndex' module once the type of assets
-- has been generalized.
--
tokenBundleHasAsset :: TokenBundle -> Asset -> Bool
tokenBundleHasAsset b = \case
    AssetLovelace -> TokenBundle.coin b /= mempty
    Asset assetId -> TokenBundle.hasQuantity b assetId

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Represents different categories of token bundles.
--
data BundleCategory asset
    = BundleWithNoAssets
    | BundleWithOneAsset asset
    | BundleWithTwoAssets (asset, asset)
    | BundleWithMultipleAssets (Set asset)
    deriving (Eq, Show)

-- | Categorizes a token bundle by how many assets it contains.
--
categorizeTokenBundle :: TokenBundle -> BundleCategory Asset
categorizeTokenBundle b = case F.toList bundleAssets of
    [      ] -> BundleWithNoAssets
    [a     ] -> BundleWithOneAsset a
    [a1, a2] -> BundleWithTwoAssets (a1, a2)
    _        -> BundleWithMultipleAssets bundleAssets
  where
    bundleAssets = tokenBundleAssets b

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
        BundleWithNoAssets -> id
        BundleWithOneAsset a -> id
            . over #indexAll (`insertEntry` a)
            . over #indexSingletons (`insertEntry` a)
        BundleWithTwoAssets (a1, a2) -> id
            . over #indexAll (`insertEntry` a1)
            . over #indexAll (`insertEntry` a2)
            . over #indexPairs (`insertEntry` a1)
            . over #indexPairs (`insertEntry` a2)
        BundleWithMultipleAssets as -> id
            . over #indexAll (flip (F.foldl' insertEntry) as)
  where
    insertEntry
        :: Ord asset
        => MonoidMap asset (Set u)
        -> asset
        -> MonoidMap asset (Set u)
    insertEntry m a = MonoidMap.adjust (Set.insert u) a m

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
    | InvariantIndexInconsistent
      -- ^ Indicates that the index sets are not consistent.
    | InvariantAssetsInconsistent
      -- ^ Indicates that the 'index' and the cached 'balance' value disagree
      --   about which assets are included.
    deriving (Eq, Show)

-- | Checks whether or not the invariant holds.
--
checkInvariant :: Ord u => UTxOIndex u -> InvariantStatus
checkInvariant i
    | BalanceIncorrect balanceError <- checkBalance i =
        InvariantBalanceError balanceError
    | not (indexIsComplete i) =
        InvariantIndexIncomplete
    | not (indexIsMinimal i) =
        InvariantIndexNonMinimal
    | not (indexIsConsistent i) =
        InvariantIndexInconsistent
    | not (assetsConsistent i) =
        InvariantAssetsInconsistent
    | otherwise =
        InvariantHolds

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
indexIsComplete i =
    F.all hasEntry $ Map.toList $ universe i
  where
    hasEntry :: (u, TokenBundle) -> Bool
    hasEntry (u, b) = case categorizeTokenBundle b of
        BundleWithNoAssets ->
            True
        BundleWithOneAsset a -> and
            [ hasEntryForAsset a indexAll
            , hasEntryForAsset a indexSingletons
            ]
        BundleWithTwoAssets (a1, a2) -> and
            [ hasEntryForAsset a1 indexAll
            , hasEntryForAsset a2 indexAll
            , hasEntryForAsset a1 indexPairs
            , hasEntryForAsset a2 indexPairs
            ]
        BundleWithMultipleAssets as ->
            F.all (`hasEntryForAsset` indexAll) as
      where
        hasEntryForAsset
            :: Ord asset
            => asset
            -> (UTxOIndex u -> MonoidMap asset (Set u))
            -> Bool
        hasEntryForAsset asset assetsMap =
            Set.member u $ MonoidMap.get asset $ assetsMap i

-- | Checks that every indexed entry is required by some entry in the 'universe'
--   map.
--
indexIsMinimal :: forall u. Ord u => UTxOIndex u -> Bool
indexIsMinimal i = F.and
    [ indexAll i
        & MonoidMap.toList
        & F.all (\(a, u) -> F.all (entryHasAsset a) u)
    , indexSingletons i
        & MonoidMap.toList
        & F.all (\(a, u) -> F.all (entryHasOneAsset a) u)
    , indexPairs i
        & MonoidMap.toList
        & F.all (\(a, u) -> F.all (entryHasTwoAssetsWith a) u)
    ]
  where
    entryHasAsset :: Asset -> u -> Bool
    entryHasAsset a = entryMatches (`tokenBundleHasAsset` a)

    entryHasOneAsset :: Asset -> u -> Bool
    entryHasOneAsset a = entryMatches $ \b -> and
        [ b `tokenBundleHasAsset` a
        , tokenBundleAssetCount b == 1
        ]

    entryHasTwoAssetsWith :: Asset -> u -> Bool
    entryHasTwoAssetsWith a = entryMatches $ \b -> and
        [ b `tokenBundleHasAsset` a
        , tokenBundleAssetCount b == 2
        ]

    entryMatches :: (TokenBundle -> Bool) -> u -> Bool
    entryMatches test u = maybe False test $ Map.lookup u $ universe i

-- | Checks that index set relationships are correct.
--
indexIsConsistent :: Ord u => UTxOIndex u -> Bool
indexIsConsistent i = F.and
    [ indexSingletons i
        `MonoidMap.disjoint` indexPairs i
    , indexSingletons i
        `MonoidMap.isSubmapOf` indexAll i
    , indexPairs i
        `MonoidMap.isSubmapOf` indexAll i
    ]

-- | Checks that the asset sets are consistent.
--
-- In particular, the set of assets in the cached 'balance' must be:
--
--    - equal to the set of assets in 'indexAll'
--    - a superset of the set of assets in 'indexSingletons'.
--    - a superset of the set of assets in 'indexPairs'.
--
assetsConsistent :: UTxOIndex u -> Bool
assetsConsistent i = and
    [ MonoidMap.nonNullKeys (indexAll i)
        == balanceAssets
    , MonoidMap.nonNullKeys (indexSingletons i)
        `Set.isSubsetOf` balanceAssets
    , MonoidMap.nonNullKeys (indexPairs i)
        `Set.isSubsetOf` balanceAssets
    ]
  where
    balanceAssets = tokenBundleAssets (balance i)
