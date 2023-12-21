{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2023 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
module Data.Percentage
    ( Percentage
    , MkPercentageError(..)
    , mkPercentage
    , getPercentage
    , clipToPercentage
    , complementPercentage
    , percentageToDouble
    ) where

import Prelude

import Control.Arrow
    ( left
    )
import Control.DeepSeq
    ( NFData
    )
import Control.Monad
    ( unless
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , withScientific
    )
import Data.Scientific
    ( FPFormat (Fixed)
    , Scientific (..)
    , formatScientific
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import Data.Text.Read
    ( rational
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Quiet
    ( Quiet (..)
    )

import qualified Data.Text as T

-- | Opaque Haskell type to represent values between 0 and 100 (incl).
newtype Percentage = Percentage
    { getPercentage :: Rational }
    deriving stock (Generic, Eq, Ord)
    deriving Show via Quiet Percentage

instance NoThunks Percentage

instance NFData Percentage

instance Buildable Percentage where
    build = build . toText

instance ToJSON Percentage where
    toJSON =
        toJSON
        . rationalToToScientific percentageNumberOfFractionalDigits
        . (* 100)
        . getPercentage

instance FromJSON Percentage where
    parseJSON = withScientific "Percentage [0,100]" $ \s ->
        either (fail . show) return
        . mkPercentage
        . toRational
        $ s / 100

instance Bounded Percentage where
    minBound = Percentage 0
    maxBound = Percentage 1

instance ToText Percentage where
    toText =
        (<> "%")
        . T.pack
        . showS
        . rationalToToScientific percentageNumberOfFractionalDigits
        . (* 100)
        . getPercentage
      where
        showS = formatScientific
            Fixed
            (Just percentageNumberOfFractionalDigits)

instance FromText Percentage where
    fromText txt = do
        (p, u) <- left (const err) $ rational txt
        unless (u == "%") $ Left err
        left (const err) . mkPercentage $ p / 100
      where
        err = TextDecodingError
            "expected a value between 0 and 100 with a '%' suffix (e.g. '14%')"

-- | Safe constructor for 'Percentage'
--
-- Takes an input in the range [0, 1].
mkPercentage
    :: Rational
    -> Either MkPercentageError Percentage
mkPercentage r
    | r < 0 =
        Left PercentageOutOfBoundsError
    | r > 1 =
        Left PercentageOutOfBoundsError
    | otherwise =
        pure . Percentage $ r

data MkPercentageError
    = PercentageOutOfBoundsError
    deriving (Show, Eq)

-- | Safe way to make a 'Percentage' by clipping values that are
-- out of bounds.
clipToPercentage :: Rational -> Percentage
clipToPercentage = Percentage . min 1 . max 0

-- | The complement is the amount that is missing to make it 100%.
--
-- Example: The 'complementPercentage' of 0.7 is 0.3.
complementPercentage :: Percentage -> Percentage
complementPercentage (Percentage p) = Percentage (1-p)

-- | Desired number of digits after the decimal point for presenting the
-- @Percentage@ type.
percentageNumberOfFractionalDigits :: Int
percentageNumberOfFractionalDigits = 2

-- | Round a @Rational@ to the given amount of fractional digits.
--
-- Note: This is safe to call on repeating digits, in contrast to @fromRational@
-- (for creating a @Scientific@).
rationalToToScientific :: Int -> Rational -> Scientific
rationalToToScientific fracDigits x = conv i / conv factor
  where
    i :: Int
    i = round (factor * x)

    conv :: (Real a, Fractional b) => a -> b
    conv = fromRational . toRational

    factor = 10 ^ fracDigits

-- | Turn a @Percentage@ to a @Double@ (without any extra rounding.)
percentageToDouble :: Percentage -> Double
percentageToDouble = fromRational . toRational . getPercentage
