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

      -- * Tests
    , isNonZero
    , isZero

    ) where

import Prelude hiding
    ( pred, subtract, succ )

import Control.DeepSeq
    ( NFData (..) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..) )
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

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance NFData TokenQuantity

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

subtract :: TokenQuantity -> TokenQuantity -> TokenQuantity
subtract (TokenQuantity x) (TokenQuantity y) = TokenQuantity $ x - y

pred :: TokenQuantity -> TokenQuantity
pred (TokenQuantity q) = TokenQuantity $ Prelude.pred q

succ :: TokenQuantity -> TokenQuantity
succ (TokenQuantity q) = TokenQuantity $ Prelude.succ q

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

isNonZero :: TokenQuantity -> Bool
isNonZero = (/= zero)

isZero :: TokenQuantity -> Bool
isZero = (== zero)
