{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Provides internal functions for the 'UTxOIndex' type, which indexes a UTxO
-- set by asset ID.
--
-- The index makes it possible to efficiently compute the subset of a UTxO
-- set containing a particular asset, or to select a UTxO entry containing
-- that asset, without having to search through the entire UTxO set.
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

    -- * Queries
    , assets
    , balance
    , lookup
    , member
    , null
    , size

    -- * Selection
    , SelectionFilter (..)
    , selectRandom

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
    ( lookup, null )

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
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
-- The index provides a mapping from assets to sets of transaction inputs.
-- A transaction input appears in the set if and only if its corresponding
-- output contains a non-zero quantity of that asset.
--
-- The index makes it possible to efficiently compute the subset of a UTxO
-- set containing a particular asset, or to select a UTxO entry containing
-- that asset, without having to search through the entire UTxO set.
--
-- The index also keeps track of the current UTxO balance for all assets,
-- making it possible to efficiently look up the quantity of a particular
-- asset without having to sum across the entire UTxO set.
--
-- The UTxO index data structure has an invariant that can be checked with
-- the 'checkInvariant' function.
--
data UTxOIndex = UTxOIndex
    { index
        :: !(Map AssetId (NonEmptySet TxIn))
    , balance
        :: !TokenBundle
    , utxo
        :: !(Map TxIn TxOut)
    }
    deriving (Eq, Generic, Read, Show)

instance NFData UTxOIndex

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | An index with no entries.
--
empty :: UTxOIndex
empty = UTxOIndex
    { index = Map.empty
    , balance = TokenBundle.empty
    , utxo = Map.empty
    }

-- | Creates a singleton index from the specified input and output.
--
singleton :: TxIn -> TxOut -> UTxOIndex
singleton i o = insertUnsafe i o empty

-- | Constructs an index from a sequence of entries.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
-- If the given sequence contains more than one entry for the same transaction
-- input, the entry that appears latest in the sequence will take precendence,
-- and all others will be ignored.
--
fromSequence :: Foldable f => f (TxIn, TxOut) -> UTxOIndex
fromSequence = flip insertMany empty

-- | Constructs an index from an ordinary 'UTxO' set.
--
-- Note that this operation is potentially expensive as it must construct an
-- index from scratch, and therefore should only be used sparingly.
--
fromUTxO :: UTxO -> UTxOIndex
fromUTxO = Map.foldlWithKey' (\u i o -> insertUnsafe i o u) empty . getUTxO

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts an index to a list of its constituent entries.
--
-- Consider using 'fold' if your goal is to consume all entries in the output.
--
toList :: UTxOIndex -> [(TxIn, TxOut)]
toList = fold (\ios i o -> (i, o) : ios) []

-- | Converts an index into an ordinary 'UTxO' set.
--
-- Consider using 'fold' if your goal is to consume all entries in the output.
--
toUTxO :: UTxOIndex -> UTxO
toUTxO = UTxO . utxo

--------------------------------------------------------------------------------
-- Folding
--------------------------------------------------------------------------------

-- | Folds strictly over the constituent entries of an index.
--
fold :: (a -> TxIn -> TxOut -> a) -> a -> UTxOIndex -> a
fold f a = Map.foldlWithKey' f a . utxo

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Inserts an entry that maps the given input to the given output.
--
-- If the index has an existing entry for the specified input, the output
-- referred to by that entry will be replaced with the specified output.
--
insert :: TxIn -> TxOut -> UTxOIndex -> UTxOIndex
insert i o = insertUnsafe i o . delete i

-- | Inserts multiple entries into an index.
--
-- See 'insert'.
--
insertMany :: Foldable f => f (TxIn, TxOut) -> UTxOIndex -> UTxOIndex
insertMany = flip $ F.foldl' $ \u (i, o) -> insert i o u

-- | Deletes the entry corresponding to the given input.
--
-- If the index has no existing entry for the specified input, the result
-- of applying this function will be equivalent to the identity function.
--
delete :: TxIn -> UTxOIndex -> UTxOIndex
delete i u = case Map.lookup i (utxo u) of
    Nothing -> u
    Just o -> UTxOIndex
        { index = F.foldl' deleteEntry (index u) (txOutAssets o)
          -- This operation is safe, since we have already determined that the
          -- entry is a member of the index, and therefore the balance must be
          -- greater than or equal to the value of this output:
        , balance = balance u `TokenBundle.unsafeSubtract` view #tokens o
        , utxo = Map.delete i $ utxo u
        }
  where
    txOutAssets :: TxOut -> Set AssetId
    txOutAssets = TokenBundle.getAssets . view #tokens

    deleteEntry
        :: Map AssetId (NonEmptySet TxIn)
        -> AssetId
        -> Map AssetId (NonEmptySet TxIn)
    deleteEntry m a = Map.update (NonEmptySet.delete i) a m

-- | Deletes multiple entries from an index.
--
-- See 'delete'.
--
deleteMany :: Foldable f => f TxIn -> UTxOIndex -> UTxOIndex
deleteMany = flip $ F.foldl' $ \u i -> delete i u

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Returns the complete set of all assets contained in an index.
--
assets :: UTxOIndex -> Set AssetId
assets = Map.keysSet . index

-- | Returns the output corresponding to the given input, if one exists.
--
-- If the index has no such input, this function returns 'Nothing'.
--
lookup :: TxIn -> UTxOIndex -> Maybe TxOut
lookup i = Map.lookup i . utxo

-- | Returns 'True' if (and only if) the index has an entry for the given input.
--
member :: TxIn -> UTxOIndex -> Bool
member i = isJust . lookup i

-- | Returns 'True' if (and only if) the index is empty.
--
null :: UTxOIndex -> Bool
null = (== 0) . size

-- | Returns the total number of UTxO entries held within the index.
--
size :: UTxOIndex -> Int
size = Map.size . utxo

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

-- | Specifies a filter for selecting UTxO entries.
--
data SelectionFilter
    = Any
        -- ^ Select any UTxO entry from the entire set.
    | WithAsset AssetId
        -- ^ Select any UTxO entry that has a non-zero quantity of the specified
        -- asset.
    deriving (Eq, Show)

-- | Selects an entry at random from the index according to the given filter.
--
-- Returns the selected entry and an updated index with the entry removed.
--
-- Returns 'Nothing' if there were no matching entries.
--
selectRandom
    :: MonadRandom m
    => UTxOIndex
    -> SelectionFilter
    -> m (Maybe ((TxIn, TxOut), UTxOIndex))
selectRandom u selectionFilter =
    (lookupAndRemoveEntry =<<) <$> selectRandomSetMember selectionSet
  where
    lookupAndRemoveEntry :: TxIn -> Maybe ((TxIn, TxOut), UTxOIndex)
    lookupAndRemoveEntry i =
        (\o -> ((i, o), delete i u)) <$> Map.lookup i (utxo u)

    selectionSet :: Set TxIn
    selectionSet = case selectionFilter of
        Any -> Map.keysSet $ utxo u
        WithAsset a -> entriesWithAsset a u

--------------------------------------------------------------------------------
-- Internal Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Returns the set of keys for entries that have non-zero quantities of the
--   given asset.
--
entriesWithAsset :: AssetId -> UTxOIndex -> Set TxIn
entriesWithAsset a = maybe mempty NonEmptySet.toSet . Map.lookup a . index

-- Inserts an entry, but without checking the following pre-condition:
--
-- Pre-condition: there is no existing entry for the specified input.
--
-- See 'insert' for a safe version of this function.
--
insertUnsafe :: TxIn -> TxOut -> UTxOIndex -> UTxOIndex
insertUnsafe i o u = UTxOIndex
    { index = F.foldl' insertEntry (index u) (txOutAssets o)
    , balance = balance u `TokenBundle.add` view #tokens o
    , utxo = Map.insert i o $ utxo u
    }
  where
    txOutAssets :: TxOut -> Set AssetId
    txOutAssets = TokenBundle.getAssets . view #tokens

    insertEntry
        :: Map AssetId (NonEmptySet TxIn)
        -> AssetId
        -> Map AssetId (NonEmptySet TxIn)
    insertEntry m a =
        Map.alter (maybe (Just createNew) (Just . updateOld)) a m
      where
        createNew = NonEmptySet.singleton i
        updateOld = NonEmptySet.insert i

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
    | InvariantBalanceError BalanceError
    | InvariantIndexIncomplete
    | InvariantIndexNonMinimal
    | InvariantAssetsInconsistent
    deriving (Eq, Show)

-- | Checks whether or not the invariant holds.
--
checkInvariant :: UTxOIndex -> InvariantStatus
checkInvariant u
    | balanceStatus /= BalanceCorrect =
        InvariantBalanceError balanceError
    | not (indexIsComplete u) =
        InvariantIndexIncomplete
    | not (indexIsMinimal u) =
        InvariantIndexNonMinimal
    | not (assetsConsistent u) =
        InvariantAssetsInconsistent
    | otherwise =
        InvariantHolds
  where
    balanceStatus = checkBalance u
    BalanceIncorrect balanceError = balanceStatus

-- | Indicates whether on not the stored balance is correct.
--
data BalanceStatus
    = BalanceCorrect
    | BalanceIncorrect BalanceError
    deriving (Eq, Show)

-- | Indicates that the stored balance is not correct.
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
checkBalance :: UTxOIndex -> BalanceStatus
checkBalance u
    | balanceComputed == balanceStored =
        BalanceCorrect
    | otherwise =
        BalanceIncorrect $ BalanceError {balanceComputed, balanceStored}
  where
    balanceComputed = F.foldMap (view #tokens) (utxo u)
    balanceStored = balance u

-- | Checks that every entry in the UTxO has a corresponding set of entries in
--   the index.
--
indexIsComplete :: UTxOIndex -> Bool
indexIsComplete u = F.all entryIndexed $ Map.toList $ utxo u
  where
    entryIndexed :: (TxIn, TxOut) -> Bool
    entryIndexed (i, o) =
        F.all (indexed i) (TokenBundle.getAssets $ view #tokens o)

    indexed :: TxIn -> AssetId -> Bool
    indexed i asset =
        case Map.lookup asset (index u) of
            Nothing -> False
            Just is -> NonEmptySet.member i is

-- | Checks that every entry in the index is required by some entry in the UTxO.
--
indexIsMinimal :: UTxOIndex -> Bool
indexIsMinimal u = F.all assetIsMinimal $ Map.toList $ index u
  where
    assetIsMinimal :: (AssetId, NonEmptySet TxIn) -> Bool
    assetIsMinimal (asset, is) =
        F.all (entryHasAsset asset) is

    entryHasAsset :: AssetId -> TxIn -> Bool
    entryHasAsset asset i =
        case Map.lookup i (utxo u) of
            Nothing ->
                False
            Just o ->
                Set.member asset $ TokenBundle.getAssets $ view #tokens o

-- | Checks that the set of assets in the cached 'balance' is equal to the set
--   of assets in the index.
--
assetsConsistent :: UTxOIndex -> Bool
assetsConsistent u =
    Map.keysSet (index u) == TokenBundle.getAssets (balance u)
