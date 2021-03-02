{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'TokenBundle' type, which combines a 'Coin' (lovelace) value
--   with a map of named token quantities, scoped by token policy.
--
-- This module is meant to be imported qualified. For example:
--
-- >>> import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
--
module Cardano.Wallet.Primitive.Types.TokenBundle
    (
    -- * Types
      TokenBundle (..)
    , AssetId (..)

    -- * Construction
    , empty
    , fromFlatList
    , fromNestedList
    , fromNestedMap
    , fromTokenMap
    , fromTokenQuantity

    -- * Deconstruction
    , toFlatList
    , toNestedList
    , toNestedMap

    -- * Coins
    , fromCoin
    , toCoin
    , isCoin
    , getCoin
    , setCoin

    -- * Arithmetic
    , add
    , subtract
    , difference

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

    -- * Queries
    , getAssets

    -- * Unsafe operations
    , unsafeSubtract

    ) where

import Prelude hiding
    ( subtract )

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), Flat (..), Nested (..), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( guard )
import Data.Bifunctor
    ( first )
import Data.Functor
    ( ($>) )
import Data.Hashable
    ( Hashable )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Map.Strict.NonEmptyMap
    ( NonEmptyMap )
import Data.Maybe
    ( fromMaybe )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), Builder, blockMapF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Combines a 'Coin' (lovelace) value with a map of named token quantities,
--   grouped by token policy.
--
data TokenBundle = TokenBundle
    { coin
        :: !Coin
    , tokens
        :: !TokenMap
    }
    deriving stock (Eq, Generic, Read, Show)
    deriving anyclass (NFData, Hashable)

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

instance PartialOrd TokenBundle where
    b1 `leq` b2 = (&&)
        (coin b1 <= coin b2)
        (tokens b1 `leq` tokens b2)

instance Semigroup TokenBundle where
    (<>) = add

instance Monoid TokenBundle where
    mempty = empty

--------------------------------------------------------------------------------
-- Text serialization
--------------------------------------------------------------------------------

instance Buildable (Flat TokenBundle) where
    build = buildBundle Flat . getFlat

instance Buildable (Nested TokenBundle) where
    build = buildBundle Nested . getNested

buildBundle
    :: Buildable (style TokenMap)
    => (TokenMap -> style TokenMap)
    -> TokenBundle
    -> Builder
buildBundle style TokenBundle {coin, tokens} = buildMap
    [ ("coin"
      , build coin)
    , ("tokens"
      , build $ style tokens)
    ]

buildMap :: [(String, Builder)] -> Builder
buildMap = blockMapF . fmap (first $ id @String)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | The empty token bundle.
--
empty :: TokenBundle
empty = TokenBundle (Coin 0) mempty

-- | Creates a token bundle from a coin and a flat list of token quantities.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant bundle.
--
fromFlatList
    :: Coin
    -> [(AssetId, TokenQuantity)]
    -> TokenBundle
fromFlatList c = TokenBundle c . TokenMap.fromFlatList

-- | Creates a token bundle from a coin and a nested list of token quantities.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant bundle.
--
fromNestedList
    :: Coin
    -> [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))]
    -> TokenBundle
fromNestedList c = TokenBundle c . TokenMap.fromNestedList

fromNestedMap
    :: (Coin, Map TokenPolicyId (NonEmptyMap TokenName TokenQuantity))
    -> TokenBundle
fromNestedMap (c, m) = TokenBundle c (TokenMap.fromNestedMap m)

fromTokenMap :: TokenMap -> TokenBundle
fromTokenMap = TokenBundle (Coin 0)

fromTokenQuantity :: AssetId -> TokenQuantity -> TokenBundle
fromTokenQuantity a q = TokenBundle (Coin 0) (TokenMap.singleton a q)

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a token bundle to a coin and a flat list of token quantities.
--
toFlatList :: TokenBundle -> (Coin, [(AssetId, TokenQuantity)])
toFlatList (TokenBundle c m) = (c, TokenMap.toFlatList m)

-- | Converts a token bundle to a coin and a nested list of token quantities.
--
toNestedList
    :: TokenBundle
    -> (Coin, [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))])
toNestedList (TokenBundle c m) = (c, TokenMap.toNestedList m)

-- | Converts a token bundle to a coin and a nested map.
--
toNestedMap
    :: TokenBundle
    -> (Coin, Map TokenPolicyId (NonEmptyMap TokenName TokenQuantity))
toNestedMap (TokenBundle c m) = (c, TokenMap.toNestedMap m)

--------------------------------------------------------------------------------
-- Coins
--------------------------------------------------------------------------------

-- | Creates a singleton token bundle from an ada 'Coin' value.
--
fromCoin :: Coin -> TokenBundle
fromCoin c = TokenBundle c mempty

-- | Coerces a token bundle to an ada 'Coin' value.
--
-- Returns a coin if (and only if) the token bundle has no other tokens.
--
toCoin :: TokenBundle -> Maybe Coin
toCoin (TokenBundle c ts)
    | TokenMap.isEmpty ts = Just c
    | otherwise = Nothing

-- | Indicates 'True' if (and only if) a token bundle has no tokens other than
--   an ada 'Coin' value.
--
isCoin :: TokenBundle -> Bool
isCoin (TokenBundle _ m) = TokenMap.isEmpty m

-- | Gets the current ada 'Coin' value from a token bundle.
--
-- If you need to assert that a bundle has no other tokens, consider using the
-- 'toCoin' function instead.
--
getCoin :: TokenBundle -> Coin
getCoin (TokenBundle c _) = c

-- | Sets the current ada 'Coin' value for a token bundle.
--
setCoin :: TokenBundle -> Coin -> TokenBundle
setCoin b c = b { coin = c }

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

-- | Adds one token bundle to another.
--
add :: TokenBundle -> TokenBundle -> TokenBundle
add (TokenBundle (Coin c1) m1) (TokenBundle (Coin c2) m2) =
    TokenBundle (Coin $ c1 + c2) (TokenMap.add m1 m2)

-- | Subtracts the second token bundle from the first.
--
-- Returns 'Nothing' if the second bundle is not less than or equal to the first
-- bundle when compared with the `leq` function.
--
subtract :: TokenBundle -> TokenBundle -> Maybe TokenBundle
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
-- >>> let oneToken = fromFlatList coin [(aid, TokenQuantity 1)]
-- >>> (mempty `difference` oneToken) `add` oneToken
-- oneToken
difference :: TokenBundle -> TokenBundle -> TokenBundle
difference (TokenBundle c1 m1) (TokenBundle c2 m2) =
    TokenBundle
        (fromMaybe (Coin 0) $ Coin.subtractCoin c1 c2)
        (TokenMap.difference m1 m2)
--------------------------------------------------------------------------------
-- Quantities
--------------------------------------------------------------------------------

-- | Gets the quantity associated with a given asset.
--
-- If the given bundle does not have an entry for the specified asset, this
-- function returns a value of zero.
--
getQuantity :: TokenBundle -> AssetId -> TokenQuantity
getQuantity = TokenMap.getQuantity . tokens

-- | Updates the quantity associated with a given asset.
--
-- If the given quantity is zero, the resultant bundle will not have an entry
-- for the given asset.
--
setQuantity :: TokenBundle -> AssetId -> TokenQuantity -> TokenBundle
setQuantity b a q = b { tokens = TokenMap.setQuantity (tokens b) a q }

-- | Returns true if and only if the given bundle has a non-zero quantity
--   for the given asset.
--
hasQuantity :: TokenBundle -> AssetId -> Bool
hasQuantity = TokenMap.hasQuantity . tokens

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
adjustQuantity b a f = b { tokens = TokenMap.adjustQuantity (tokens b) a f }

-- | Removes the quantity associated with the given asset.
--
-- This is equivalent to calling 'setQuantity' with a value of zero.
--
removeQuantity :: TokenBundle -> AssetId -> TokenBundle
removeQuantity b a = b { tokens = TokenMap.removeQuantity (tokens b) a }

--------------------------------------------------------------------------------
-- Policies
--------------------------------------------------------------------------------

-- | Returns 'True' if (and only if) there is at least one non-zero token
--   quantity corresponding to the specified policy.
--
hasPolicy :: TokenBundle -> TokenPolicyId -> Bool
hasPolicy = TokenMap.hasPolicy . tokens

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

getAssets :: TokenBundle -> Set AssetId
getAssets = TokenMap.getAssets . tokens

--------------------------------------------------------------------------------
-- Unsafe operations
--------------------------------------------------------------------------------

-- | Subtracts the second token bundle from the first.
--
-- Pre-condition: the second bundle is less than or equal to the first bundle
-- when compared with the `leq` function.
--
-- Throws a run-time exception if the pre-condition is violated.
--
unsafeSubtract :: TokenBundle -> TokenBundle -> TokenBundle
unsafeSubtract (TokenBundle (Coin c1) m1) (TokenBundle (Coin c2) m2) =
    TokenBundle (Coin $ c1 - c2) (TokenMap.unsafeSubtract m1 m2)
