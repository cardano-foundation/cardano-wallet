{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the 'Coin' data type, which represents a quantity of
-- lovelace.
--
module Cardano.Wallet.Primitive.Types.Coin
    ( -- * Type
      Coin (..)

      -- * Conversions (Safe)
    , fromIntegralMaybe
    , fromNatural
    , fromQuantity
    , fromWord64
    , toInteger
    , toNatural
    , toQuantity
    , toQuantityMaybe
    , toWord64Maybe

      -- * Conversions (Unsafe)
    , unsafeFromIntegral
    , unsafeToQuantity
    , unsafeToWord64

      -- * Arithmetic operations
    , add
    , subtract
    , difference
    , distance

      -- * Partitioning
    , equipartition
    , partition
    , partitionDefault
    , unsafePartition
    ) where

import Prelude hiding
    ( fromIntegral, subtract, toInteger )

import Cardano.Numeric.Util
    ( equipartitionNatural, partitionNatural )
import Control.DeepSeq
    ( NFData (..) )
import Data.Bits
    ( Bits )
import Data.Hashable
    ( Hashable )
import Data.IntCast
    ( IsIntSubType, intCast, intCastMaybe )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Cancellative
    ( LeftReductive, Reductive, RightReductive )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, RightGCDMonoid )
import Data.Monoid.Monus
    ( Monus, OverlappingGCDMonoid )
import Data.Monoid.Null
    ( MonoidNull )
import Data.Quantity
    ( Quantity (..) )
import Data.Semigroup.Commutative
    ( Commutative )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), fixedF )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Data.Text as T
import qualified Prelude

-- | A 'Coin' represents a quantity of lovelace.
--
-- Reminder: 1 ada = 1,000,000 lovelace.
--
-- The 'Coin' type has 'Semigroup' and 'Monoid' instances that correspond
-- to ordinary addition and summation.
--
newtype Coin = Coin
    { unCoin :: Natural
    }
    deriving stock (Ord, Eq, Generic)
    deriving (Read, Show) via Quiet Coin
    deriving (Commutative, Semigroup, Monoid, MonoidNull) via Sum Natural
    deriving (LeftReductive, RightReductive, Reductive) via Sum Natural
    deriving (LeftGCDMonoid, RightGCDMonoid, GCDMonoid) via Sum Natural
    deriving (OverlappingGCDMonoid, Monus) via Sum Natural

instance ToText Coin where
    toText (Coin c) = T.pack $ show c

instance FromText Coin where
    fromText = fmap Coin . fromText @Natural

instance NFData Coin
instance Hashable Coin

instance Buildable Coin where
    build (Coin c) = fixedF @Double 6 (Prelude.fromIntegral c / 1e6)

--------------------------------------------------------------------------------
-- Conversions (Safe)
--------------------------------------------------------------------------------

-- | Constructs a 'Coin' from an 'Integral' value.
--
-- Returns 'Nothing' if the given value is negative.
--
fromIntegralMaybe :: (Bits i, Integral i) => i -> Maybe Coin
fromIntegralMaybe i = Coin <$> intCastMaybe i

-- | Constructs a 'Coin' from a 'Natural' value.
--
fromNatural :: Natural -> Coin
fromNatural = Coin

-- | Constructs a 'Coin' from a 'Quantity'.
--
fromQuantity
    :: (Integral i, IsIntSubType i Natural ~ 'True)
    => Quantity "lovelace" i
    -> Coin
fromQuantity (Quantity c) = Coin (intCast c)

-- | Constructs a 'Coin' from a 'Word64' value.
--
fromWord64 :: Word64 -> Coin
fromWord64 = Coin . intCast

-- | Converts a 'Coin' to an 'Integer' value.
--
toInteger :: Coin -> Integer
toInteger = intCast . unCoin

-- | Converts a 'Coin' to a 'Natural' value.
--
toNatural :: Coin -> Natural
toNatural = unCoin

-- | Converts a 'Coin' to a 'Quantity'.
--
toQuantity
    :: (Integral i, IsIntSubType Natural i ~ 'True)
    => Coin
    -> Quantity "lovelace" i
toQuantity (Coin c) = Quantity (intCast c)

-- | Converts a 'Coin' to a 'Quantity'.
--
-- Returns 'Nothing' if the given value does not fit within the bounds of
-- the target type.
--
toQuantityMaybe
    :: (Bits i, Integral i)
    => Coin
    -> Maybe (Quantity "lovelace" i)
toQuantityMaybe (Coin c) = Quantity <$> intCastMaybe c

-- | Converts a 'Coin' to a 'Word64' value.
--
-- Returns 'Nothing' if the given value does not fit within the bounds of a
-- 64-bit word.
--
toWord64Maybe :: Coin -> Maybe Word64
toWord64Maybe (Coin c) = intCastMaybe c

--------------------------------------------------------------------------------
-- Conversions (Unsafe)
-------------------------------------------------------------------------------

-- | Constructs a 'Coin' from an 'Integral' value.
--
-- Callers of this function must take responsibility for checking that the
-- given value is not negative.
--
-- Produces a run-time error if the given value is negative.
--
unsafeFromIntegral
    :: HasCallStack
    => (Bits i, Integral i, Show i)
    => i
    -> Coin
unsafeFromIntegral i = fromMaybe onError (fromIntegralMaybe i)
  where
    onError =  error $ unwords
        [ "Coin.unsafeFromIntegral:"
        , show i
        , "is not a natural number."
        ]

-- | Converts a 'Coin' to a 'Quantity'.
--
-- Callers of this function must take responsibility for checking that the
-- given value will fit within the bounds of the target type.
--
-- Produces a run-time error if the given value is out of bounds.
--
unsafeToQuantity
    :: HasCallStack
    => (Bits i, Integral i)
    => Coin
    -> Quantity "lovelace" i
unsafeToQuantity c = fromMaybe onError (toQuantityMaybe c)
  where
    onError = error $ unwords
        [ "Coin.unsafeToQuantity:"
        , show c
        , "does not fit within the bounds of the target type."
        ]

-- | Converts a 'Coin' to a 'Word64' value.
--
-- Callers of this function must take responsibility for checking that the
-- given value will fit within the bounds of a 64-bit word.
--
-- Produces a run-time error if the given value is out of bounds.
--
unsafeToWord64 :: HasCallStack => Coin -> Word64
unsafeToWord64 c = fromMaybe onError (toWord64Maybe c)
  where
    onError = error $ unwords
        [ "Coin.unsafeToWord64:"
        , show c
        , "does not fit within the bounds of a 64-bit word."
        ]

--------------------------------------------------------------------------------
-- Arithmetic operations
--------------------------------------------------------------------------------

-- | Subtracts the second coin from the first.
--
-- Returns 'Nothing' if the second coin is strictly greater than the first.
--
subtract :: Coin -> Coin -> Maybe Coin
subtract (Coin a) (Coin b)
    | a >= b    = Just $ Coin (a - b)
    | otherwise = Nothing

-- | Calculates the combined value of two coins.
--
add :: Coin -> Coin -> Coin
add (Coin a) (Coin b) = Coin (a + b)

-- | Subtracts the second coin from the first.
--
-- Returns 'Coin 0' if the second coin is strictly greater than the first.
--
difference :: Coin -> Coin -> Coin
difference a b = fromMaybe (Coin 0) (subtract a b)

-- | Absolute difference between two coin amounts. The result is never negative.
distance :: Coin -> Coin -> Coin
distance (Coin a) (Coin b) = if a < b then Coin (b - a) else Coin (a - b)

--------------------------------------------------------------------------------
-- Partitioning
--------------------------------------------------------------------------------

-- | Computes the equipartition of a coin into 'n' smaller coins.
--
-- An /equipartition/ of a coin is a /partition/ of that coin into 'n' smaller
-- coins whose values differ by no more than 1.
--
-- The resultant list is sorted in ascending order.
--
equipartition
    :: Coin
    -- ^ The coin to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the coin.
    -> NonEmpty Coin
    -- ^ The partitioned coins.
equipartition c =
    fmap fromNatural . equipartitionNatural (toNatural c)

-- | Partitions a coin into a number of parts, where the size of each part is
--   proportional (modulo rounding) to the size of its corresponding element in
--   the given list of weights, and the number of parts is equal to the number
--   of weights.
--
-- Returns 'Nothing' if the sum of weights is equal to zero.
--
partition
    :: Coin
    -- ^ The coin to be partitioned.
    -> NonEmpty Coin
    -- ^ The list of weights.
    -> Maybe (NonEmpty Coin)
    -- ^ The partitioned coins.
partition c
    = fmap (fmap fromNatural)
    . partitionNatural (toNatural c)
    . fmap toNatural

-- | Partitions a coin into a number of parts, where the size of each part is
--   proportional (modulo rounding) to the size of its corresponding element in
--   the given list of weights, and the number of parts is equal to the number
--   of weights.
--
-- This function always satisfies the following properties:
--
-- prop> fold   (partitionDefault c ws) == c
-- prop> length (partitionDefault c ws) == length ws
--
-- If the sum of weights is equal to zero, then this function returns an
-- 'equipartition' satisfying the following property:
--
-- prop> partitionDefault c ws == equipartition c ws
--
partitionDefault
    :: Coin
    -- ^ The token quantity to be partitioned.
    -> NonEmpty Coin
    -- ^ The list of weights.
    -> NonEmpty Coin
    -- ^ The partitioned token quantities.
partitionDefault c ws = fromMaybe (equipartition c ws) (partition c ws)

-- | Partitions a coin into a number of parts, where the size of each part is
--   proportional (modulo rounding) to the size of its corresponding element in
--   the given list of weights, and the number of parts is equal to the number
--   of weights.
--
-- Throws a run-time error if the sum of weights is equal to zero.
--
unsafePartition
    :: HasCallStack
    => Coin
    -- ^ The coin to be partitioned.
    -> NonEmpty Coin
    -- ^ The list of weights.
    -> NonEmpty Coin
    -- ^ The partitioned coins.
unsafePartition = (fromMaybe zeroWeightSumError .) . partition
  where
    zeroWeightSumError = error
        "Coin.unsafePartition: weights must have a non-zero sum."
