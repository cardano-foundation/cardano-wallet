{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
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
      TokenBundle (..)
    , AssetId (..)

    -- * Construction
    , fromFlatList
    , fromNestedList

    -- * Deconstruction
    , toFlatList
    , toNestedList

    -- * Coins
    , fromCoin
    , toCoin
    , isCoin
    , getCoin
    , setCoin

    -- * Arithmetic
    , add

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
    ( negate, null )

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
import Data.Bifunctor
    ( first )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Fmt
    ( Buildable (..), Builder, blockMapF )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )

import qualified Cardano.Wallet.Primitive.Types.TokenMap as TM

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
    deriving stock (Eq, Generic, Show)

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

-- | Creates a token bundle from a coin and a flat list of token quantities.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant bundle.
--
fromFlatList
    :: Coin
    -> [(AssetId, TokenQuantity)]
    -> TokenBundle
fromFlatList c = TokenBundle c . TM.fromFlatList

-- | Creates a token bundle from a coin and a nested list of token quantities.
--
-- If a token name appears more than once in the list under the same policy,
-- its associated quantities will be added together in the resultant bundle.
--
fromNestedList
    :: Coin
    -> [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))]
    -> TokenBundle
fromNestedList c = TokenBundle c . TM.fromNestedList

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

-- | Converts a token bundle to a coin and a flat list of token quantities.
--
toFlatList :: TokenBundle -> (Coin, [(AssetId, TokenQuantity)])
toFlatList (TokenBundle c m) = (c, TM.toFlatList m)

-- | Converts a token bundle to a coin and a nested list of token quantities.
--
toNestedList
    :: TokenBundle
    -> (Coin, [(TokenPolicyId, NonEmpty (TokenName, TokenQuantity))])
toNestedList (TokenBundle c m) = (c, TM.toNestedList m)

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
    | TM.isEmpty ts = Just c
    | otherwise = Nothing

-- | Indicates 'True' if (and only if) a token bundle has no tokens other than
--   an ada 'Coin' value.
--
isCoin :: TokenBundle -> Bool
isCoin (TokenBundle _ m) = TM.isEmpty m

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
    TokenBundle (Coin $ c1 + c2) (TM.add m1 m2)

--------------------------------------------------------------------------------
-- Quantities
--------------------------------------------------------------------------------

-- | Gets the quantity associated with a given asset.
--
-- If the given bundle does not have an entry for the specified asset, this
-- function returns a value of zero.
--
getQuantity :: TokenBundle -> AssetId -> TokenQuantity
getQuantity = TM.getQuantity . tokens

-- | Updates the quantity associated with a given asset.
--
-- If the given quantity is zero, the resultant bundle will not have an entry
-- for the given asset.
--
setQuantity :: TokenBundle -> AssetId -> TokenQuantity -> TokenBundle
setQuantity b a q = b { tokens = TM.setQuantity (tokens b) a q }

-- | Returns true if and only if the given bundle has a non-zero quantity
--   for the given asset.
--
hasQuantity :: TokenBundle -> AssetId -> Bool
hasQuantity = TM.hasQuantity . tokens

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
adjustQuantity b a f = b { tokens = TM.adjustQuantity (tokens b) a f }

-- | Removes the quantity associated with the given asset.
--
-- This is equivalent to calling 'setQuantity' with a value of zero.
--
removeQuantity :: TokenBundle -> AssetId -> TokenBundle
removeQuantity b a = b { tokens = TM.removeQuantity (tokens b) a }

--------------------------------------------------------------------------------
-- Policies
--------------------------------------------------------------------------------

-- | Returns 'True' if (and only if) there is at least one non-zero token
--   quantity corresponding to the specified policy.
--
hasPolicy :: TokenBundle -> TokenPolicyId -> Bool
hasPolicy = TM.hasPolicy . tokens
