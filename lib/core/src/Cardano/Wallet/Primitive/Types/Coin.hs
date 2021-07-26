{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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
    , coinQuantity
    , coinToInteger
    , coinToNatural

      -- * Checks
    , isValidCoin

      -- * Arithmetic operations
    , addCoin
    , subtractCoin
    , sumCoins
    , distance

      -- * Partitioning
    , equipartition
    , partition
    , unsafePartition

    ) where

import Prelude

import Cardano.Numeric.Util
    ( equipartitionNatural, partitionNatural )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Data.Foldable
    ( foldl' )
import Data.Hashable
    ( Hashable )
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
import Overloaded.Numerals
    ( FromNumeral (..) )
import Quiet
    ( Quiet (..) )

import qualified Data.Text as T

-- | A 'Coin' represents a quantity of lovelace.
--
-- Reminder: 1 ada = 1,000,000 lovelace.
--
-- NOTE: The 'Coin' value is stored as a 64-bit unsigned integer. The maximum
-- supply of lovelace is less than 2^56, so there is ample space to store any
-- circulating amount of Ada.
--
-- However be careful when summing coins, for example, if calculating historical
-- volumes traded, because this may overflow.
newtype Coin = Coin
    { unCoin :: Word64
    }
    deriving stock (Ord, Eq, Generic)
    deriving (Read, Show) via (Quiet Coin)
    deriving (FromNumeral n) via Word64

instance Semigroup Coin where
    -- Word64 doesn't have a default Semigroup instance.
    Coin a <> Coin b = Coin (a + b)

instance Monoid Coin where
    mempty = Coin 0

instance ToText Coin where
    toText (Coin c) = T.pack $ show c

instance FromText Coin where
    fromText = validate <=< (fmap (Coin . fromIntegral) . fromText @Natural)
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
    build (Coin c) = fixedF @Double 6 (fromIntegral c / 1e6)

-- | Compatibility function to use while 'Quantity' is still used in non-API
-- parts of the code.
coinQuantity :: Integral a => Coin -> Quantity n a
coinQuantity (Coin n) = Quantity (fromIntegral n)

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin

coinToNatural :: Coin -> Natural
coinToNatural = fromIntegral . unCoin

unsafeNaturalToCoin :: Natural -> Coin
unsafeNaturalToCoin = Coin . fromIntegral

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
    -- Note: the natural-to-coin conversion is safe, as partitioning guarantees
    -- to produce values that are less than or equal to the original value.
    fmap unsafeNaturalToCoin . equipartitionNatural (coinToNatural c)

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
    -- Note: the natural-to-coin conversion is safe, as partitioning guarantees
    -- to produce values that are less than or equal to the original value.
    = fmap (fmap unsafeNaturalToCoin)
    . partitionNatural (coinToNatural c)
    . fmap coinToNatural

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
