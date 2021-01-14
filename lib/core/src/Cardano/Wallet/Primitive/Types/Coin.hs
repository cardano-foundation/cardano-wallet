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
    (
    -- * Type
      Coin (..)

    -- * Operations
    , addCoin
    , subtractCoin

    -- * Checks
    , isValidCoin
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), fixedF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Data.Text as T

-- | A 'Coin' represents a quantity of lovelace.
--
-- Reminder: 1 ada = 1,000,000 lovelace.
--
newtype Coin = Coin
    { unCoin :: Word64
    }
    deriving stock (Ord, Eq, Generic)
    deriving (Read, Show) via (Quiet Coin)

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

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin 45_000_000_000_000_000

instance Buildable Coin where
    build (Coin c) = fixedF @Double 6 (fromIntegral c / 1e6)

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Subtracts the second coin from the first.
--
-- Returns 'Nothing' if the second coin is strictly greater than the first.
--
subtractCoin :: Coin -> Coin -> Maybe Coin
subtractCoin (Coin a) (Coin b)
    | a >= b    = Just $ Coin (a - b)
    | otherwise = Nothing

-- | Adds the given coins together.
--
-- NOTE: It is generally safe to add coins and stay in the same domain because
-- the max supply is known (45B), which easily fits within a 'Word64'. So for
-- the vast majority of usages of this function within cardano-wallet, it is a
-- safe operation.
--
addCoin :: Coin -> Coin -> Coin
addCoin (Coin a) (Coin b) = Coin (a + b)

--------------------------------------------------------------------------------
-- Checks
--------------------------------------------------------------------------------

isValidCoin :: Coin -> Bool
isValidCoin c = c >= minBound && c <= maxBound
