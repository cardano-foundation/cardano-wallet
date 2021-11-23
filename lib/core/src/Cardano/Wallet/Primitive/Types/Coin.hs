{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

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
    , fromIntegral
    , fromNatural
    , fromWord64
    , toInteger
    , toNatural
    , toQuantity
    , toWord64

      -- * Conversions (Unsafe)
    , unsafeFromIntegral
    , unsafeToQuantity
    , unsafeToWord64

      -- * Checks
    , isValidCoin

      -- * Arithmetic operations
    , addCoin
    , subtractCoin
    , sumCoins
    , difference
    , distance

      -- * Partitioning
    , equipartition
    , partition
    , unsafePartition

    ) where

import Prelude hiding
    ( fromIntegral, toInteger )

import Cardano.Numeric.Util
    ( equipartitionNatural, partitionNatural )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Data.Bits
    ( Bits )
import Data.Foldable
    ( foldl' )
import Data.Hashable
    ( Hashable )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
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
newtype Coin = Coin
    { unCoin :: Natural
    }
    deriving stock (Ord, Eq, Generic)
    deriving (Read, Show) via (Quiet Coin)

instance Semigroup Coin where
    -- Natural doesn't have a default Semigroup instance.
    Coin a <> Coin b = Coin (a + b)

instance Monoid Coin where
    mempty = Coin 0

instance ToText Coin where
    toText (Coin c) = T.pack $ show c

instance FromText Coin where
    fromText = validate <=< (fmap Coin . fromText @Natural)
      where
        validate x
            | isValidCoin x =
                return x
            | otherwise =
                Left $ TextDecodingError "Coin value is out of bounds"

instance NFData Coin
instance Hashable Coin

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin 45_000_000_000_000_000

instance Buildable Coin where
    build (Coin c) = fixedF @Double 6 (Prelude.fromIntegral c / 1e6)

--------------------------------------------------------------------------------
-- Conversions (Safe)
--------------------------------------------------------------------------------

-- | Constructs a 'Coin' from an 'Integral' value.
--
-- Returns 'Nothing' if the given value is negative.
--
fromIntegral :: (Bits i, Integral i) => i -> Maybe Coin
fromIntegral i = Coin <$> intCastMaybe i

-- | Constructs a 'Coin' from a 'Natural' value.
--
fromNatural :: Natural -> Coin
fromNatural = Coin

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
-- Returns 'Nothing' if the given value does not fit within the bounds of
-- the target type.
--
toQuantity :: (Bits i, Integral i) => Coin -> Maybe (Quantity n i)
toQuantity (Coin c) = Quantity <$> intCastMaybe c

-- | Converts a 'Coin' to a 'Word64' value.
--
-- Returns 'Nothing' if the given value does not fit within the bounds of a
-- 64-bit word.
--
toWord64 :: Coin -> Maybe Word64
toWord64 (Coin c) = intCastMaybe c

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
unsafeFromIntegral i = fromMaybe onError (fromIntegral i)
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
    -> Quantity n i
unsafeToQuantity c = fromMaybe onError (toQuantity c)
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
unsafeToWord64 c = fromMaybe onError (toWord64 c)
  where
    onError = error $ unwords
        [ "Coin.unsafeToWord64:"
        , show c
        , "does not fit within the bounds of a 64-bit word."
        ]

{-------------------------------------------------------------------------------
                                     Checks
-------------------------------------------------------------------------------}

-- | Whether the coin amount is less than the total amount of Ada.
isValidCoin :: Coin -> Bool
isValidCoin c = c >= minBound && c <= maxBound

{-------------------------------------------------------------------------------
                                   Operations
-------------------------------------------------------------------------------}

-- | Subtracts the second coin from the first.
--
-- Returns 'Nothing' if the second coin is strictly greater than the first.
--
subtractCoin :: Coin -> Coin -> Maybe Coin
subtractCoin (Coin a) (Coin b)
    | a >= b    = Just $ Coin (a - b)
    | otherwise = Nothing

-- | Calculate the combined value of two coins.
--
-- NOTE: It is generally safe to add coins and stay in the same domain because
-- the max supply is known (45B), which easily fits within a 'Word64'. So for
-- the vast majority of usages of this function within cardano-wallet, it is a
-- safe operation.
--
addCoin :: Coin -> Coin -> Coin
addCoin (Coin a) (Coin b) = Coin (a + b)

-- | Add a list of coins together.
sumCoins :: Foldable t => t Coin -> Coin
sumCoins = foldl' addCoin (Coin 0)

-- | Subtracts the second coin from the first.
--
-- Returns 'Coin 0' if the second coin is strictly greater than the first.
--
difference :: Coin -> Coin -> Coin
difference a b = fromMaybe (Coin 0) (subtractCoin a b)

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
--   proportional to the size of its corresponding element in the given list
--   of weights, and the number of parts is equal to the number of weights.
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
--   proportional to the size of its corresponding element in the given list
--   of weights, and the number of parts is equal to the number of weights.
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
