{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TokenMap' type, which represents a map of named non-ada
--   token quantities scoped by token policy.
--
-- The 'TokenMap' type does not provide a way to store ada quantities. If you
-- also need to store ada quantities, use the 'TokenBundle' type.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Cardano.Wallet.Primitive.Types.TokenMap as TM
--
module Cardano.Wallet.Primitive.Types.TokenMap
    (
    -- * Types

      -- Important:
      --
      -- The default data constructor for 'TokenMap' is not exported, by design,
      -- as the internal data structure has an invariant that must be preserved
      -- across all operations.
      --
      -- Exporting the default constructor would make it possible for functions
      -- outside the 'TokenMap' module to break the invariant, opening the door
      -- to subtle regressions.
      --
      -- See the definition of 'TokenMap' for more details of the invariant.
      --
      -- To construct a 'TokenMap', use one of the provided constructors, all
      -- of which are tested to check that they respect the invariant.
      --
      TokenMap
    , AssetId (..)

    -- * Construction
    , empty
    , singleton
    , fromFlatList
    , fromNestedList
    , fromNestedMap

    -- * Deconstruction
    , toFlatList
    , toNestedList
    , toNestedMap

    -- * Filtering
    , filter

    -- * Arithmetic
    , add
    , subtract
    , difference
    , intersection

    -- * Tests
    , isEmpty
    , isNotEmpty

    -- * Quantities
    , getQuantity
    , setQuantity
    , hasQuantity
    , adjustQuantity
    , removeQuantity
    , maximumQuantity

    -- * Partitioning
    , equipartitionAssets
    , equipartitionQuantities
    , equipartitionQuantitiesWithUpperBound

    -- * Policies
    , hasPolicy

    -- * Serialization
    , Flat (..)
    , Nested (..)

    -- * Queries
    , getAssets

    -- * Unsafe operations
    , unsafeSubtract

    ) where

import Prelude hiding
    ( filter, subtract )

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Numeric.Util
    ( equipartitionNatural )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( guard, when, (<=<) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), camelTo2, genericParseJSON, genericToJSON )
import Data.Aeson.Types
    ( Options (..), Parser )
import Data.Bifunctor
    ( first )
import Data.Functor
    ( ($>) )
import Data.Hashable
    ( Hashable (..), hashUsing )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Map.Strict.NonEmptyMap
    ( NonEmptyMap )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Ratio
    ( (%) )
import Data.Set
    ( Set )
import Data.Text.Class
    ( toText )
import Fmt
    ( Buildable (..), Builder, blockListF', blockMapF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TokenQuantity
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NEMap
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A map of named token quantities, grouped by token policy.
--
-- The token map data structure has an important invariant: all token
-- quantities held within a map are non-zero.
--
-- This means that:
--
--   - using the 'setQuantity' function to add a zero-valued quantity to a
--     map is equivalent to applying the identity operation to that map.
--
--   - using the 'setQuantity' function to change an existing quantity to zero
--     is equivalent to removing that quantity from the map.
--
-- As a consequence of this invariant, the token map data structure is
-- always in its canonical form: we can perform an equality check without
-- needing any extra canonicalization steps.
--
newtype TokenMap = TokenMap
    { unTokenMap
        :: Map TokenPolicyId (NonEmptyMap TokenName TokenQuantity)
    }
    deriving stock (Eq, Generic)
    deriving (Read, Show) via (Quiet TokenMap)

-- | Token maps can be partially ordered, but there is no total ordering of
--   token maps that's consistent with their arithmetic properties.
--
-- In the event that someone attempts to define an 'Ord' instance for the
-- 'TokenMap' type, we generate a type error.
--
-- If some arbitrary ordering is needed (for example, so that token maps can
-- be included in an ordered set), the recommended course of action is to
-- define a newtype with its own dedicated 'Ord' instance.
--
instance TypeError ('Text "Ord not supported for token maps")
        => Ord TokenMap where
    compare = error "Ord not supported for token maps"

-- | Partial ordering for token maps.
--
-- There is no total ordering of token maps that's consistent with their
-- arithmetic properties.
--
-- To see why this is true, consider how we might order the following maps:
--
--     >>> p = fromFlatList [(assetA, 2), (assetB, 1)]
--     >>> q = fromFlatList [(assetA, 1), (assetB, 2)]
--
-- One possibility would be to use a lexicographic ordering, but this is not
-- arithmetically useful.
--
-- Instead, we define a partial order, where map 'x' is less than or equal
-- to map 'y' if (and only if):
--
--     - all the quantities in map 'x' are less than or equal to their
--       corresponding quantities in map 'y';
--
--     - all the quantities in map 'y' are greater than or equal to their
--       corresponding quantities in map 'x'.
--
-- For example, consider the following pair of maps:
--
--     >>> x = fromFlatList [(assetA, 1)]
--     >>> y = fromFlatList [(assetA, 2), (assetB, 1)]
--
-- In the above example, map 'x' is strictly less than map 'y'.
--
instance PartialOrd TokenMap where
    m1 `leq` m2 = F.all
        (\a -> getQuantity m1 a <= getQuantity m2 a)
        (getAssets m1 `Set.union` getAssets m2)

instance NFData TokenMap
instance Hashable TokenMap where
    hashWithSalt = hashUsing (Map.toList . unTokenMap)

instance Semigroup TokenMap where
    (<>) = add

instance Monoid TokenMap where
    mempty = empty

-- | A combination of a token policy identifier and a token name that can be
--   used as a compound identifier.
--
data AssetId = AssetId
    { tokenPolicyId
        :: !TokenPolicyId
    , tokenName
        :: !TokenName
    }
    deriving stock (Eq, Generic, Ord, Read, Show)

instance NFData AssetId

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

-- | When used with the 'Buildable' or 'ToJSON' instances, provides a flat
-- serialization style, where token quantities are paired with their asset
-- identifiers.
--
newtype Flat a = Flat { getFlat :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Flat a))

-- | When used with the 'Buildable' or 'ToJSON' instances, provides a nested
-- serialization style, where token quantities are grouped by policy
-- identifier.
--
newtype Nested a = Nested { getNested :: a }
    deriving stock (Eq, Generic, Ord)
    deriving Show via (Quiet (Nested a))

--------------------------------------------------------------------------------
-- Text serialization
--------------------------------------------------------------------------------

instance Buildable (Flat TokenMap) where
    build = buildTokenMap . getFlat
      where
        buildTokenMap =
            buildList buildAssetQuantity . toFlatList
        buildAssetQuantity (AssetId policy token, quantity) = buildMap
            [ ("policy",
                build policy)
            , ("token",
                build token)
            , ("quantity",
                build quantity)
            ]

instance Buildable (Nested TokenMap) where
    build = buildTokenMap . unTokenMap . getNested
      where
        buildTokenMap =
            buildList buildPolicy . Map.toList
        buildPolicy (policy, assetMap) = buildMap
            [ ("policy",
                build policy)
            , ("tokens",
                buildList buildTokenQuantity (NEMap.toList assetMap))
            ]
        buildTokenQuantity (token, quantity) = buildMap
            [ ("token",
                build token)
            , ("quantity",
                build quantity)
            ]

buildList :: Foldable f => (a -> Builder) -> f a -> Builder
buildList = blockListF' "-"

buildMap :: [(String, Builder)] -> Builder
buildMap = blockMapF . fmap (first $ id @String)

--------------------------------------------------------------------------------
-- JSON serialization (common)
--------------------------------------------------------------------------------

jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_') }

jsonFailWith :: String -> Parser a
jsonFailWith s = fail $
    "Error while deserializing token map from JSON: " <> s <> "."

jsonFailWithEmptyTokenList :: TokenPolicyId -> Parser a
jsonFailWithEmptyTokenList policy = jsonFailWith $ unwords
    [ "Encountered empty token list for policy"
    , show (toText policy)
    ]

jsonFailWithZeroValueTokenQuantity :: TokenPolicyId -> TokenName -> Parser a
jsonFailWithZeroValueTokenQuantity policy token = jsonFailWith $ unwords
    [ "Encountered zero-valued quantity for token"
    , show (toText token)
    , "within policy"
    , show (toText policy)
    ]

--------------------------------------------------------------------------------
-- JSON serialization (flat)
--------------------------------------------------------------------------------

instance ToJSON (Flat TokenMap) where
    toJSON = toJSON . fmap fromTuple . toFlatList . getFlat
      where
        fromTuple (AssetId p t, q) = FlatAssetQuantity p t q

instance FromJSON (Flat TokenMap) where
    parseJSON =
        fmap (Flat . fromFlatList) . mapM parseTuple <=< parseJSON
      where
        parseTuple :: FlatAssetQuantity -> Parser (AssetId, TokenQuantity)
        parseTuple (FlatAssetQuantity p t q) = do
            when (TokenQuantity.isZero q) $
                jsonFailWithZeroValueTokenQuantity p t
            pure (AssetId p t, q)

-- Used for JSON serialization only: not exported.
data FlatAssetQuantity = FlatAssetQuantity
    { _policyId :: !TokenPolicyId
    , _assetName :: !TokenName
    , _quantity :: !TokenQuantity
    } deriving Generic

instance FromJSON FlatAssetQuantity where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON FlatAssetQuantity where
    toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- JSON serialization (nested)
--------------------------------------------------------------------------------

instance ToJSON (Nested TokenMap) where
    toJSON = toJSON . fmap mapOuter . toNestedList . getNested
      where
        mapOuter = uncurry NestedMapEntry . fmap mapInner
        mapInner = NE.toList . fmap (uncurry NestedTokenQuantity)

instance FromJSON (Nested TokenMap) where
    parseJSON = parseEntryList <=< parseJSON @[NestedMapEntry]
      where
        parseEntryList :: [NestedMapEntry] -> Parser (Nested TokenMap)
        parseEntryList = fmap (Nested . fromNestedList) . mapM parseEntry

        parseEntry
            :: NestedMapEntry
            -> Parser (TokenPolicyId, NonEmpty (TokenName, TokenQuantity))
        parseEntry (NestedMapEntry policy mTokens) = do
            tokens <- maybe (jsonFailWithEmptyTokenList policy) pure $
                NE.nonEmpty mTokens
            (policy,) <$> mapM (parseToken policy) tokens

        parseToken
            :: TokenPolicyId
            -> NestedTokenQuantity
            -> Parser (TokenName, TokenQuantity)
        parseToken policy (NestedTokenQuantity token quantity) = do
            when (TokenQuantity.isZero quantity) $
                jsonFailWithZeroValueTokenQuantity policy token
            pure (token, quantity)

-- Used for JSON serialization only: not exported.
data NestedMapEntry = NestedMapEntry
    { _policyId :: !TokenPolicyId
    , _tokens :: ![NestedTokenQuantity]
    } deriving Generic

-- Used for JSON serialization only: not exported.
data NestedTokenQuantity = NestedTokenQuantity
    { _assetName :: !TokenName
    , _quantity :: !TokenQuantity
    } deriving Generic

instance FromJSON NestedMapEntry where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON NestedMapEntry where
    toJSON = genericToJSON jsonOptions

instance FromJSON NestedTokenQuantity where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON NestedTokenQuantity where
    toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | The empty token map.
--
empty :: TokenMap
empty = TokenMap mempty

-- | Creates a singleton token map with just one token quantity.
--
-- If the specified token quantity is zero, then the resultant map will be
-- equal to the 'empty' map.
--
singleton :: AssetId -> TokenQuantity -> TokenMap
singleton = setQuantity empty

-- | Creates a token map from a flat list.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant map.
--
fromFlatList :: [(AssetId, TokenQuantity)] -> TokenMap
fromFlatList = F.foldl' acc empty
  where
    acc b (asset, quantity) = adjustQuantity b asset (<> quantity)

-- | Creates a token map from a nested list.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant map.
--
fromNestedList
    :: [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))] -> TokenMap
fromNestedList entries = fromFlatList
    [ (AssetId policy token, quantity)
    | (policy, tokenQuantities) <- entries
    , (token, quantity) <- NE.toList tokenQuantities
    ]

-- | Creates a token map from a nested map.
--
fromNestedMap
    :: Map TokenPolicyId (NonEmptyMap TokenName TokenQuantity)
    -> TokenMap
fromNestedMap = fromNestedList . Map.toList . fmap NEMap.toList

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a token map to a flat list.
--
toFlatList :: TokenMap -> [(AssetId, TokenQuantity)]
toFlatList b =
    [ (AssetId policy token, quantity)
    | (policy, tokenQuantities) <- toNestedList b
    , (token, quantity) <- NE.toList tokenQuantities
    ]

-- | Converts a token map to a nested list.
--
toNestedList
    :: TokenMap -> [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))]
toNestedList =
    fmap (fmap NEMap.toList) . Map.toList . unTokenMap

-- | Converts a token map to a nested map.
--
toNestedMap
    :: TokenMap
    -> Map TokenPolicyId (NonEmptyMap TokenName TokenQuantity)
toNestedMap = unTokenMap

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

filter :: (AssetId -> Bool) -> TokenMap -> TokenMap
filter f = fromFlatList . L.filter (f . fst) . toFlatList

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

-- | Adds one token map to another.
--
add :: TokenMap -> TokenMap -> TokenMap
add a b = F.foldl' acc a $ toFlatList b
  where
    acc c (asset, quantity) =
        adjustQuantity c asset (`TokenQuantity.add` quantity)

-- | Subtracts the second token map from the first.
--
-- Returns 'Nothing' if the second map is not less than or equal to the first
-- map when compared with the `leq` function.
--
subtract :: TokenMap -> TokenMap -> Maybe TokenMap
subtract a b = guard (b `leq` a) $> unsafeSubtract a b

-- | Analogous to @Set.difference@, return the difference between two token
-- maps.
--
-- The following property holds:
-- prop> x `leq` (x `difference` y) `add` y
--
-- Note that there's a `leq` rather than equality, which we'd expect if this was
-- subtraction of integers. I.e.
--
-- >>> (0 - 1) + 1
-- 0
--
-- whereas
--
-- >>> let oneToken = singleton aid (TokenQuantity 1)
-- >>> (mempty `difference` oneToken) `add` oneToken
-- oneToken
difference :: TokenMap -> TokenMap -> TokenMap
difference m1 m2 = L.foldl' reduce m1 (toFlatList m2)
  where
    reduce :: TokenMap -> (AssetId, TokenQuantity) -> TokenMap
    reduce m (a, q) = adjustQuantity m a (`TokenQuantity.difference` q)

-- | Computes the intersection of two token maps.
--
-- Analogous to @Set.intersection@.
--
-- Example:
--
-- >>> m1 = [("a", 1), ("b", 2), ("c", 3)          ]
-- >>> m2 = [          ("b", 3), ("c", 2), ("d", 1)]
-- >>> intersection m1 m2
--          [          ("b", 2), ("c", 2)          ]
--
intersection :: TokenMap -> TokenMap -> TokenMap
intersection m1 m2 =
    fromFlatList (getMinimumQuantity <$> F.toList sharedAssets)
  where
    getMinimumQuantity :: AssetId -> (AssetId, TokenQuantity)
    getMinimumQuantity a = (a, ) $ min
        (getQuantity m1 a)
        (getQuantity m2 a)

    sharedAssets :: Set AssetId
    sharedAssets = Set.intersection (getAssets m1) (getAssets m2)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- | Returns true if and only if the given map is empty.
--
isEmpty :: TokenMap -> Bool
isEmpty = (== empty)

-- | Returns true if and only if the given map is not empty.
--
isNotEmpty :: TokenMap -> Bool
isNotEmpty = (/= empty)

--------------------------------------------------------------------------------
-- Quantities
--------------------------------------------------------------------------------

-- | Gets the quantity associated with a given asset.
--
-- If the given map does not have an entry for the specified asset, this
-- function returns a value of zero.
--
getQuantity :: TokenMap -> AssetId -> TokenQuantity
getQuantity (TokenMap m) (AssetId policy token) =
    fromMaybe TokenQuantity.zero $ NEMap.lookup token =<< Map.lookup policy m

-- | Updates the quantity associated with a given asset.
--
-- If the given quantity is zero, the resultant map will not have an entry for
-- the given asset.
--
setQuantity :: TokenMap -> AssetId -> TokenQuantity -> TokenMap
setQuantity originalMap@(TokenMap m) (AssetId policy token) quantity =
    case getPolicyMap originalMap policy of
        Nothing | TokenQuantity.isZero quantity ->
            originalMap
        Nothing ->
            createPolicyMap
        Just policyMap | TokenQuantity.isZero quantity ->
            removeQuantityFromPolicyMap policyMap
        Just policyMap ->
            updateQuantityInPolicyMap policyMap
  where
    createPolicyMap = TokenMap
        $ flip (Map.insert policy) m
        $ NEMap.singleton token quantity

    removeQuantityFromPolicyMap policyMap =
        case NEMap.delete token policyMap of
            Nothing ->
                TokenMap $ Map.delete policy m
            Just newPolicyMap ->
                TokenMap $ Map.insert policy newPolicyMap m

    updateQuantityInPolicyMap policyMap = TokenMap
        $ flip (Map.insert policy) m
        $ NEMap.insert token quantity policyMap

-- | Returns true if and only if the given map has a non-zero quantity for the
--   given asset.
--
hasQuantity :: TokenMap -> AssetId -> Bool
hasQuantity (TokenMap m) (AssetId policy token) =
    isJust $ NEMap.lookup token =<< Map.lookup policy m

-- | Uses the specified function to adjust the quantity associated with a
--   given asset.
--
-- If the result of adjusting the quantity is equal to zero, the resultant map
-- will not have an entry for the given asset.
--
adjustQuantity
    :: TokenMap
    -> AssetId
    -> (TokenQuantity -> TokenQuantity)
    -> TokenMap
adjustQuantity m asset adjust =
    setQuantity m asset $ adjust $ getQuantity m asset

-- | Removes the quantity associated with the given asset.
--
-- This is equivalent to calling 'setQuantity' with a value of zero.
--
removeQuantity :: TokenMap -> AssetId -> TokenMap
removeQuantity m asset = setQuantity m asset TokenQuantity.zero

-- | Get the largest quantity from this map.
--
maximumQuantity :: TokenMap -> TokenQuantity
maximumQuantity =
    Map.foldl' (\a -> Map.foldr findMaximum a . NEMap.toMap) zero . unTokenMap
  where
    zero :: TokenQuantity
    zero = TokenQuantity 0

    findMaximum :: TokenQuantity -> TokenQuantity -> TokenQuantity
    findMaximum challenger champion
        | challenger > champion =
            challenger
        | otherwise =
            champion

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

-- | Partitions a token map into 'n' smaller maps, where the asset sets of the
--   resultant maps are disjoint.
--
-- In the resultant maps, the smallest asset set size and largest asset set
-- size will differ by no more than 1.
--
-- The quantities of each asset are unchanged.
--
equipartitionAssets
    :: TokenMap
    -- ^ The token map to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the token map.
    -> NonEmpty TokenMap
    -- ^ The partitioned maps.
equipartitionAssets m mapCount =
    fromFlatList <$> NE.unfoldr generateChunk (assetCounts, toFlatList m)
  where
    -- The total number of assets.
    assetCount :: Int
    assetCount = Set.size $ getAssets m

    -- How many asset quantities to include in each chunk.
    assetCounts :: NonEmpty Int
    assetCounts = fromIntegral @Natural @Int <$>
        equipartitionNatural (fromIntegral @Int @Natural assetCount) mapCount

    -- Generates a single chunk of asset quantities.
    generateChunk :: (NonEmpty Int, [aq]) -> ([aq], Maybe (NonEmpty Int, [aq]))
    generateChunk (c :| mcs, aqs) = case NE.nonEmpty mcs of
        Just cs -> (prefix, Just (cs, suffix))
        Nothing -> (aqs, Nothing)
      where
        (prefix, suffix) = L.splitAt c aqs

-- | Partitions a token map into 'n' smaller maps, where the quantity of each
--   token is equipartitioned across the resultant maps.
--
-- In the resultant maps, the smallest quantity and largest quantity of a given
-- token will differ by no more than 1.
--
-- The resultant list is sorted into ascending order when maps are compared
-- with the 'leq' function.
--
equipartitionQuantities
    :: TokenMap
    -- ^ The map to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the map.
    -> NonEmpty TokenMap
    -- ^ The partitioned maps.
equipartitionQuantities m count =
    F.foldl' accumulate (empty <$ count) (toFlatList m)
  where
    accumulate
        :: NonEmpty TokenMap
        -> (AssetId, TokenQuantity)
        -> NonEmpty TokenMap
    accumulate maps (asset, quantity) = NE.zipWith (<>) maps $
        singleton asset <$>
            TokenQuantity.equipartition quantity count

-- | Partitions a token map into 'n' smaller maps, where the quantity of each
--   token is equipartitioned across the resultant maps, with the goal that no
--   token quantity in any of the resultant maps exceeds the given upper bound.
--
-- The value 'n' is computed automatically, and is the minimum value required
-- to achieve the goal that no token quantity in any of the resulting maps
-- exceeds the maximum allowable token quantity.
--
equipartitionQuantitiesWithUpperBound
    :: TokenMap
    -> TokenQuantity
    -- ^ Maximum allowable token quantity.
    -> NonEmpty TokenMap
    -- ^ The partitioned maps.
equipartitionQuantitiesWithUpperBound m (TokenQuantity maxQuantity)
    | maxQuantity == 0 =
        maxQuantityZeroError
    | currentMaxQuantity <= maxQuantity =
        m :| []
    | otherwise =
        equipartitionQuantities m (() :| replicate extraPartCount ())
  where
    TokenQuantity currentMaxQuantity = maximumQuantity m

    extraPartCount :: Int
    extraPartCount = floor $ pred currentMaxQuantity % maxQuantity

    maxQuantityZeroError = error $ unwords
        [ "equipartitionQuantitiesWithUpperBound:"
        , "the maximum allowable token quantity cannot be zero."
        ]

--------------------------------------------------------------------------------
-- Policies
--------------------------------------------------------------------------------

-- | Returns true if and only if there is at least one entry corresponding
--   to the specified policy.
--
hasPolicy :: TokenMap -> TokenPolicyId -> Bool
hasPolicy b policy = isJust $ Map.lookup policy $ unTokenMap b

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

getAssets :: TokenMap -> Set AssetId
getAssets = Set.fromList . fmap fst . toFlatList

--------------------------------------------------------------------------------
-- Unsafe operations
--------------------------------------------------------------------------------

-- | Subtracts the second token map from the first.
--
-- Pre-condition: the second map is less than or equal to the first map when
-- compared with the `leq` function.
--
-- Throws a run-time exception if the pre-condition is violated.
--
unsafeSubtract :: TokenMap -> TokenMap -> TokenMap
unsafeSubtract a b = F.foldl' acc a $ toFlatList b
  where
    acc c (asset, quantity) =
        adjustQuantity c asset (`TokenQuantity.unsafeSubtract` quantity)

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

getPolicyMap
    :: TokenMap
    -> TokenPolicyId
    -> Maybe (NonEmptyMap TokenName TokenQuantity)
getPolicyMap b policy = Map.lookup policy (unTokenMap b)
