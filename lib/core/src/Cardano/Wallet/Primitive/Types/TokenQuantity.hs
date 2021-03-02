{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenQuantity
    (
      -- * Type
      TokenQuantity (..)

      -- * Values
    , zero

      -- * Operations
    , add
    , subtract
    , pred
    , succ
    , equipartition

      -- * Tests
    , isNonZero
    , isZero

      -- * Unsafe operations
    , unsafeSubtract

    ) where

import Prelude hiding
    ( pred, subtract, succ )

import Cardano.Numeric.Util
    ( equipartitionNatural )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( guard )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
import Data.Functor
    ( ($>) )
import Data.Hashable
    ( Hashable )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Prelude

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | Represents an integral quantity of tokens.
--
-- At present, we use 'Natural' as our underlying type, as the only use case
-- for these quantities is to be included in token bundles held within
-- transaction outputs, and these must never be negative.
--
-- When we build support for minting and burning of tokens, we may wish to
-- parameterize this type and allow it to be instantiated with 'Integer'.
--
newtype TokenQuantity = TokenQuantity
    { unTokenQuantity :: Natural }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenQuantity)
    deriving anyclass (NFData, Hashable)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Semigroup TokenQuantity where
    (<>) = add

instance Monoid TokenQuantity where
    mempty = zero

instance Buildable TokenQuantity where
    build = build . toText . unTokenQuantity

instance ToText TokenQuantity where
    toText = toText . unTokenQuantity

instance FromText TokenQuantity where
    fromText = fmap (TokenQuantity . fromIntegral @Integer) . fromText

instance FromJSON TokenQuantity where
    parseJSON = fmap TokenQuantity . parseJSON
instance ToJSON TokenQuantity where
    toJSON = toJSON . unTokenQuantity

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

zero :: TokenQuantity
zero = TokenQuantity 0

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

add :: TokenQuantity -> TokenQuantity -> TokenQuantity
add (TokenQuantity x) (TokenQuantity y) = TokenQuantity $ x + y

-- | Subtracts the second token quantity from the first.
--
-- Returns 'Nothing' if the first quantity is less than the second quantity.
--
subtract :: TokenQuantity -> TokenQuantity -> Maybe TokenQuantity
subtract x y = guard (x >= y) $> unsafeSubtract x y

pred :: TokenQuantity -> TokenQuantity
pred (TokenQuantity q) = TokenQuantity $ Prelude.pred q

succ :: TokenQuantity -> TokenQuantity
succ (TokenQuantity q) = TokenQuantity $ Prelude.succ q

-- | Computes the equipartition of a token quantity into 'n' smaller quantities.
--
-- An /equipartition/ of a token quantity is a /partition/ of that quantity
-- into 'n' smaller quantities whose values differ by no more than 1.
--
-- The resultant list is sorted in ascending order.
--
equipartition
    :: TokenQuantity
    -- ^ The token quantity to be partitioned.
    -> NonEmpty a
    -- ^ Represents the number of portions in which to partition the quantity.
    -> NonEmpty TokenQuantity
    -- ^ The partitioned quantities.
equipartition q =
    fmap TokenQuantity . equipartitionNatural (unTokenQuantity q)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

isNonZero :: TokenQuantity -> Bool
isNonZero = (/= zero)

isZero :: TokenQuantity -> Bool
isZero = (== zero)

--------------------------------------------------------------------------------
-- Unsafe operations
--------------------------------------------------------------------------------

-- | Subtracts the second token quantity from the first.
--
-- Pre-condition: the first quantity is not less than the second quantity.
--
-- Throws a run-time exception if the pre-condition is violated.
--
unsafeSubtract :: TokenQuantity -> TokenQuantity -> TokenQuantity
unsafeSubtract (TokenQuantity x) (TokenQuantity y) = TokenQuantity $ x - y
