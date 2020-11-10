{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TokenBundle' type, which represents a bundle of named
--   non-zero token quantities scoped by token policy.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
--
module Cardano.Wallet.Primitive.Types.TokenBundle
    (
    -- * Types
      TokenBundle
    , AssetId (..)

    -- * Construction
    , empty
    , singleton
    , fromFlatList
    , fromNestedList

    -- * Deconstruction
    , toFlatList
    , toNestedList

    -- * Interoperability
    , adaAssetId
    , fromCoin
    , getCoin
    , setCoin

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

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
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
import Data.Map.NonEmpty.Strict
    ( NonEmptyMap )
import Data.Map.Strict
    ( Map )
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

import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as TP
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as TQ
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty.Strict as NEMap
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A map of named token quantities, grouped by token policy.
--
-- The token bundle data structure has an important invariant: all token
-- quantities held within a bundle are non-zero.
--
-- This means that:
--
--   - using the 'setQuantity' function to add a zero-valued quantity to a
--     bundle is equivalent to applying the identity operation to that bundle.
--
--   - using the 'setQuantity' function to change an existing quantity to zero
--     is equivalent to removing that quantity from the bundle.
--
-- As a consequence of this invariant, the token bundle data structure is
-- always in its canonical form: we can perform an equality check without
-- needing any extra canonicalization steps.
--
newtype TokenBundle = TokenBundle
    { unTokenBundle
        :: Map TokenPolicyId (NonEmptyMap TokenName TokenQuantity)
    }
    deriving stock (Eq, Generic)
    deriving Show via (Quiet TokenBundle)

-- | Token bundles can be partially ordered, but there is no total ordering of
--   token bundles that's consistent with their arithmetic properties.
--
-- In the event that someone attempts to define an 'Ord' instance for the
-- 'TokenBundle' type, we generate a type error.
--
-- If some arbitrary ordering is needed (for example, so that token bundles can
-- be included in an ordered set), the recommended course of action is to
-- define a newtype with its own dedicated 'Ord' instance.
--
instance TypeError ('Text "Ord not supported for token bundles")
        => Ord TokenBundle where
    compare = error "Ord not supported for token bundles"

instance NFData TokenBundle

instance Semigroup TokenBundle where
    (<>) = add

instance Monoid TokenBundle where
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

instance Buildable (Flat TokenBundle) where
    build = buildBundle . getFlat
      where
        buildBundle b = buildMap
            [ ("token-bundle",
                buildList buildAssetQuantity $ toFlatList b)
            ]
        buildAssetQuantity (_asset, _quantity) = buildMap
            [ ("asset",
                buildAssetId _asset)
            , ("quantity",
                build _quantity)
            ]
        buildAssetId (AssetId policy token) = buildMap
            [ ("policy",
                build policy)
            , ("token",
                build token)
            ]

instance Buildable (Nested TokenBundle) where
    build = buildBundle . unTokenBundle . getNested
      where
        buildBundle b = buildMap
            [ ("token-bundle",
                buildList buildPolicy $ Map.toList b)
            ]
        buildPolicy (policy, assetMap) = buildMap
            [ ("policy",
                build policy)
            , ("tokens",
                buildList buildTokenQuantity (NEMap.toList assetMap))
            ]
        buildTokenQuantity (_tokenName, _tokenQuantity) = buildMap
            [ ("name",
                build _tokenName)
            , ("quantity",
                build _tokenQuantity)
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
    "Error while deserializing token bundle from JSON: " <> s <> "."

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

instance ToJSON (Flat TokenBundle) where
    toJSON = toJSON . fmap fromTuple . toFlatList . getFlat
      where
        fromTuple (AssetId p t, q) = FlatAssetQuantity (FlatAssetId p t) q

instance FromJSON (Flat TokenBundle) where
    parseJSON =
        fmap (Flat . fromFlatList) . sequence . fmap parseTuple <=< parseJSON
      where
        parseTuple :: FlatAssetQuantity -> Parser (AssetId, TokenQuantity)
        parseTuple (FlatAssetQuantity (FlatAssetId p t) q) = do
            when (TQ.isZero q) $ jsonFailWithZeroValueTokenQuantity p t
            pure (AssetId p t, q)

-- Used for JSON serialization only: not exported.
data FlatAssetQuantity = FlatAssetQuantity
    { _asset :: !FlatAssetId
    , _quantity :: !TokenQuantity
    } deriving Generic

-- Used for JSON serialization only: not exported.
data FlatAssetId = FlatAssetId
    { _policy :: !TokenPolicyId
    , _token :: !TokenName
    } deriving Generic

instance FromJSON FlatAssetQuantity where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON FlatAssetQuantity where
    toJSON = genericToJSON jsonOptions

instance FromJSON FlatAssetId where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON FlatAssetId where
    toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- JSON serialization (nested)
--------------------------------------------------------------------------------

instance ToJSON (Nested TokenBundle) where
    toJSON = toJSON . fmap mapOuter . toNestedList . getNested
      where
        mapOuter = uncurry NestedBundleEntry . fmap mapInner
        mapInner = NE.toList . fmap (uncurry NestedTokenQuantity)

instance FromJSON (Nested TokenBundle) where
    parseJSON = parseEntryList <=< parseJSON @[NestedBundleEntry]
      where
        parseEntryList :: [NestedBundleEntry] -> Parser (Nested TokenBundle)
        parseEntryList = fmap (Nested . fromNestedList) . mapM parseEntry

        parseEntry
            :: NestedBundleEntry
            -> Parser (TokenPolicyId, NonEmpty (TokenName, TokenQuantity))
        parseEntry (NestedBundleEntry policy mTokens) = do
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
data NestedBundleEntry = NestedBundleEntry
    { _policy :: !TokenPolicyId
    , _tokens :: ![NestedTokenQuantity]
    } deriving Generic

-- Used for JSON serialization only: not exported.
data NestedTokenQuantity = NestedTokenQuantity
    { _name :: !TokenName
    , _quantity :: !TokenQuantity
    } deriving Generic

instance FromJSON NestedBundleEntry where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON NestedBundleEntry where
    toJSON = genericToJSON jsonOptions

instance FromJSON NestedTokenQuantity where
    parseJSON = genericParseJSON jsonOptions
instance ToJSON NestedTokenQuantity where
    toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | The empty token bundle.
--
empty :: TokenBundle
empty = TokenBundle mempty

-- | Creates a singleton token bundle with just one token quantity.
--
-- If the specified token quantity is zero, then the resultant bundle will be
-- equal to the 'empty' bundle.
--
singleton :: AssetId -> TokenQuantity -> TokenBundle
singleton = setQuantity empty

-- | Creates a token bundle from a flat list.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant bundle.
--
fromFlatList :: [(AssetId, TokenQuantity)] -> TokenBundle
fromFlatList = F.foldl' acc empty
  where
    acc b (asset, quantity) = adjustQuantity b asset (<> quantity)

-- | Creates a token bundle from a nested list.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant bundle.
--
fromNestedList
    :: [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))] -> TokenBundle
fromNestedList entries = fromFlatList
    [ (AssetId policy token, quantity)
    | (policy, tokenQuantities) <- entries
    , (token, quantity) <- NE.toList tokenQuantities
    ]

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a token bundle to a flat list.
--
toFlatList :: TokenBundle -> [(AssetId, TokenQuantity)]
toFlatList b =
    [ (AssetId policy token, quantity)
    | (policy, tokenQuantities) <- toNestedList b
    , (token, quantity) <- NE.toList tokenQuantities
    ]

-- | Converts a token bundle to a nested list.
--
toNestedList
    :: TokenBundle -> [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))]
toNestedList =
    fmap (fmap NEMap.toList) . Map.toList . unTokenBundle

--------------------------------------------------------------------------------
-- Interoperability
--------------------------------------------------------------------------------

-- | Represents the ada asset.
--
adaAssetId :: AssetId
adaAssetId = AssetId TP.adaTokenPolicyId TP.adaTokenName

-- | Creates a singleton token bundle from a 'Coin' value.
--
-- 'Coin' values correspond to the ada asset, represented by the 'adaAssetId'
-- constant.
--
fromCoin :: Coin -> TokenBundle
fromCoin = singleton adaAssetId . TokenQuantity . fromIntegral . unCoin

-- | Gets the current 'Coin' value from a token bundle.
--
-- 'Coin' values correspond to the ada asset, represented by the 'adaAssetId'
-- constant.
--
getCoin :: TokenBundle -> Coin
getCoin = Coin . fromIntegral . unTokenQuantity . flip getQuantity adaAssetId

-- | Sets the current 'Coin' value for a token bundle.
--
-- 'Coin' values correspond to the ada asset, represented by the 'adaAssetId'
-- constant.
--
setCoin :: TokenBundle -> Coin -> TokenBundle
setCoin bundle =
    setQuantity bundle adaAssetId . TokenQuantity . fromIntegral . unCoin

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

-- | Adds one token bundle to another.
--
add :: TokenBundle -> TokenBundle -> TokenBundle
add a b = F.foldl' acc a $ toFlatList b
  where
    acc c (asset, quantity) =
        adjustQuantity c asset (`TQ.add` quantity)

-- | Subtracts one token bundle from another.
--
subtract :: TokenBundle -> TokenBundle -> TokenBundle
subtract a b = F.foldl' acc a $ toFlatList b
  where
    acc c (asset, quantity) =
        adjustQuantity c asset (`TQ.subtract` quantity)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- | Returns true if and only if the given bundle is empty.
--
isEmpty :: TokenBundle -> Bool
isEmpty = (== empty)

-- | Returns true if and only if the given bundle is not empty.
--
isNotEmpty :: TokenBundle -> Bool
isNotEmpty = (/= empty)

--------------------------------------------------------------------------------
-- Quantities
--------------------------------------------------------------------------------

-- | Gets the quantity associated with a given asset.
--
-- If the given bundle does not have an entry for the specified asset, this
-- function returns a value of zero.
--
getQuantity :: TokenBundle -> AssetId -> TokenQuantity
getQuantity (TokenBundle bundle) (AssetId policy token) =
    fromMaybe TQ.zero $ NEMap.lookup token =<< Map.lookup policy bundle

-- | Updates the quantity associated with a given asset.
--
-- If the given quantity is zero, the resultant bundle will not have an entry
-- for the given asset.
--
setQuantity :: TokenBundle -> AssetId -> TokenQuantity -> TokenBundle
setQuantity originalBundle@(TokenBundle m) (AssetId policy token) quantity =
    case getPolicyMap originalBundle policy of
        Nothing | TQ.isZero quantity ->
            originalBundle
        Nothing ->
            createPolicyMap
        Just policyMap | TQ.isZero quantity ->
            removeQuantityFromPolicyMap policyMap
        Just policyMap ->
            updateQuantityInPolicyMap policyMap
  where
    createPolicyMap = TokenBundle
        $ flip (Map.insert policy) m
        $ NEMap.singleton token quantity

    removeQuantityFromPolicyMap policyMap =
        case NEMap.fromMap (NEMap.delete token policyMap) of
            Nothing ->
                TokenBundle $ Map.delete policy m
            Just newPolicyMap ->
                TokenBundle $ Map.insert policy newPolicyMap m

    updateQuantityInPolicyMap policyMap = TokenBundle
        $ flip (Map.insert policy) m
        $ NEMap.insert token quantity policyMap

-- | Returns true if and only if the given bundle has a non-zero quantity
--   for the given asset.
--
hasQuantity :: TokenBundle -> AssetId -> Bool
hasQuantity (TokenBundle bundle) (AssetId policy token) =
    isJust $ NEMap.lookup token =<< Map.lookup policy bundle

-- | Uses the specified function to adjust the quantity associated with a
--   given asset.
--
-- If the result of adjusting the quantity is equal to zero, the resultant
-- bundle will not have an entry for the given asset.
--
adjustQuantity
    :: TokenBundle
    -> AssetId
    -> (TokenQuantity -> TokenQuantity)
    -> TokenBundle
adjustQuantity bundle asset adjust =
    setQuantity bundle asset $ adjust $ getQuantity bundle asset

-- | Removes the quantity associated with the given asset.
--
-- This is equivalent to calling 'setQuantity' with a value of zero.
--
removeQuantity :: TokenBundle -> AssetId -> TokenBundle
removeQuantity bundle asset = setQuantity bundle asset TQ.zero

--------------------------------------------------------------------------------
-- Policies
--------------------------------------------------------------------------------

-- | Returns true if and only if there is at least one entry corresponding
--   to the specified policy.
--
hasPolicy :: TokenBundle -> TokenPolicyId -> Bool
hasPolicy b policy = isJust $ Map.lookup policy $ unTokenBundle b

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

getPolicyMap
    :: TokenBundle
    -> TokenPolicyId
    -> Maybe (NonEmptyMap TokenName TokenQuantity)
getPolicyMap b policy = Map.lookup policy (unTokenBundle b)
