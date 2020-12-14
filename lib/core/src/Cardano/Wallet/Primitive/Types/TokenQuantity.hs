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
    , negate
    , pred
    , succ

      -- * Tests
    , isStrictlyPositive
    , isStrictlyNegative
    , isNonZero
    , isZero

    ) where

import Prelude hiding
    ( negate, pred, succ )

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
import Quiet
    ( Quiet (..) )

import qualified Prelude

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

newtype TokenQuantity = TokenQuantity
    { unTokenQuantity :: Integer }
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

negate :: TokenQuantity -> TokenQuantity
negate (TokenQuantity x) = TokenQuantity (Prelude.negate x)

pred :: TokenQuantity -> TokenQuantity
pred (TokenQuantity q) = TokenQuantity $ Prelude.pred q

succ :: TokenQuantity -> TokenQuantity
succ (TokenQuantity q) = TokenQuantity $ Prelude.succ q

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

isStrictlyNegative :: TokenQuantity -> Bool
isStrictlyNegative = (< zero)

isStrictlyPositive :: TokenQuantity -> Bool
isStrictlyPositive = (> zero)

isNonZero :: TokenQuantity -> Bool
isNonZero = (/= zero)

isZero :: TokenQuantity -> Bool
isZero = (== zero)
