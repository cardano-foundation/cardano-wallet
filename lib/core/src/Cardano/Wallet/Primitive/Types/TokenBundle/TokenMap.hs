{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TokenMap' type, which represents a map of named non-zero
--   token quantities scoped by token policy.
--
module Cardano.Wallet.Primitive.Types.TokenBundle.TokenMap
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

    -- * Deconstruction
    , toFlatList
    , toNestedList

    -- * Arithmetic
    , add
    , subtract

    -- * Tests
    , isEmpty
    , isNotEmpty

    -- * Quantities
    , getQuantity
    , setQuantity
    , hasQuantity
    , adjustQuantity
    , removeQuantity

    -- * Policies
    , hasPolicy

    -- * Serialization
    , Flat (..)
    , Nested (..)

    ) where

import Prelude hiding
    ( negate, null, subtract )

import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( mapM, when, (<=<) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), camelTo2, genericParseJSON, genericToJSON )
import Data.Aeson.Types
    ( Options (..), Parser )
import Data.Bifunctor
    ( first )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Map.Strict.NonEmptyMap
    ( NonEmptyMap )
import Data.Maybe
    ( fromMaybe, isJust )
import Data.Text.Class
    ( toText )
import Fmt
    ( Buildable (..), Builder, blockListF', blockMapF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )
import Quiet
    ( Quiet (..) )

import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TQ
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NEMap

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
    deriving Show via (Quiet TokenMap)

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

instance NFData TokenMap

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
    deriving stock (Eq, Generic, Ord, Show)

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
        buildTokenMap b = buildMap
            [ ("tokens",
                buildList buildAssetQuantity $ toFlatList b)
            ]
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
        buildTokenMap b = buildMap
            [ ("tokens",
                buildList buildPolicy $ Map.toList b)
            ]
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
        fmap (Flat . fromFlatList) . sequence . fmap parseTuple <=< parseJSON
      where
        parseTuple :: FlatAssetQuantity -> Parser (AssetId, TokenQuantity)
        parseTuple (FlatAssetQuantity p t q) = do
            when (TQ.isZero q) $ jsonFailWithZeroValueTokenQuantity p t
            pure (AssetId p t, q)

-- Used for JSON serialization only: not exported.
data FlatAssetQuantity = FlatAssetQuantity
    { _policy :: !TokenPolicyId
    , _token :: !TokenName
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
            when (TQ.isZero quantity) $
                jsonFailWithZeroValueTokenQuantity policy token
            pure (token, quantity)

-- Used for JSON serialization only: not exported.
data NestedMapEntry = NestedMapEntry
    { _policy :: !TokenPolicyId
    , _tokens :: ![NestedTokenQuantity]
    } deriving Generic

-- Used for JSON serialization only: not exported.
data NestedTokenQuantity = NestedTokenQuantity
    { _token :: !TokenName
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

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

-- | Adds one token map to another.
--
add :: TokenMap -> TokenMap -> TokenMap
add a b = F.foldl' acc a $ toFlatList b
  where
    acc c (asset, quantity) =
        adjustQuantity c asset (`TQ.add` quantity)

-- | Subtracts one token map from another.
--
subtract :: TokenMap -> TokenMap -> TokenMap
subtract a b = F.foldl' acc a $ toFlatList b
  where
    acc c (asset, quantity) =
        adjustQuantity c asset (`TQ.subtract` quantity)

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
    fromMaybe TQ.zero $ NEMap.lookup token =<< Map.lookup policy m

-- | Updates the quantity associated with a given asset.
--
-- If the given quantity is zero, the resultant map will not have an entry for
-- the given asset.
--
setQuantity :: TokenMap -> AssetId -> TokenQuantity -> TokenMap
setQuantity originalMap@(TokenMap m) (AssetId policy token) quantity =
    case getPolicyMap originalMap policy of
        Nothing | TQ.isZero quantity ->
            originalMap
        Nothing ->
            createPolicyMap
        Just policyMap | TQ.isZero quantity ->
            removeQuantityFromPolicyMap policyMap
        Just policyMap ->
            updateQuantityInPolicyMap policyMap
  where
    createPolicyMap = TokenMap
        $ flip (Map.insert policy) m
        $ NEMap.singleton token quantity

    removeQuantityFromPolicyMap policyMap =
        case NEMap.fromMap (NEMap.delete token policyMap) of
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
removeQuantity m asset = setQuantity m asset TQ.zero

--------------------------------------------------------------------------------
-- Policies
--------------------------------------------------------------------------------

-- | Returns true if and only if there is at least one entry corresponding
--   to the specified policy.
--
hasPolicy :: TokenMap -> TokenPolicyId -> Bool
hasPolicy b policy = isJust $ Map.lookup policy $ unTokenMap b

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

getPolicyMap
    :: TokenMap
    -> TokenPolicyId
    -> Maybe (NonEmptyMap TokenName TokenQuantity)
getPolicyMap b policy = Map.lookup policy (unTokenMap b)
