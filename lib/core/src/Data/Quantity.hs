{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Representation of values with an associated (free) unit of measure. Useful to
-- disambiguate primitive types like 'Int' or 'String' which can be in different
-- bases depending on the context.

module Data.Quantity
    ( -- * Polymorphic Quantity
      Quantity(..)

      -- * Percentage
    , Percentage
    , MkPercentageError(..)
    , mkPercentage
    , getPercentage
    , clipToPercentage
    , percentageToDouble
    ) where

import Prelude

import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( unless )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (String)
    , object
    , withObject
    , withScientific
    , (.:)
    , (.=)
    )
import Data.Aeson.Types
    ( Parser )
import Data.Hashable
    ( Hashable )
import Data.Proxy
    ( Proxy (..) )
import Data.Scientific
    ( FPFormat (Fixed), Scientific (..), formatScientific )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Text.Read
    ( rational )
import Fmt
    ( Buildable (..), fmt )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import NoThunks.Class
    ( NoThunks (..) )
import Quiet
    ( Quiet (..) )

import qualified Data.Text as T


-- | @Quantity (unit :: Symbol) a@ is a primitive @a@  multiplied by an @unit@.
--
-- Example:
--
-- Instead of providing the unit implicitly as a comment, or a part of a name
--
-- >>> a :: Word32 -- in lovelace
--
-- we can write
--
-- >>> a :: Quantity "lovelace" Word32
--
-- which now has a different type from
--
-- >>> b :: Quantity "lovelace/byte" Word32
--
-- so mixing them up is more difficult.
--
-- The unit is mostly a phantom type, but it is also included in the
-- @ToJSON@/@FromJSON@ instances.
--
-- >>> Aeson.encode $ Quantity @"lovelace" 14
-- {"unit":"lovelace","quantity":14}
newtype Quantity (unit :: Symbol) a = Quantity { getQuantity :: a }
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Bounded, Enum, Hashable)
    deriving Show via (Quiet (Quantity unit a))

instance NoThunks a => NoThunks (Quantity unit a)

instance Functor (Quantity any) where
    fmap f (Quantity a) = Quantity (f a)

instance NFData a => NFData (Quantity unit a)

instance (KnownSymbol unit, ToJSON a) => ToJSON (Quantity unit a) where
    toJSON (Quantity a) = object
        [ "unit"     .= symbolVal (Proxy :: Proxy unit)
        , "quantity" .= toJSON a
        ]

instance (KnownSymbol unit, FromJSON a) => FromJSON (Quantity unit a) where
    parseJSON = withObject "Quantity" $ \o -> do
        verifyUnit (Proxy :: Proxy unit) =<< o .: "unit"
        Quantity <$> o .: "quantity"
      where
        verifyUnit :: Proxy (unit :: Symbol) -> Value -> Parser ()
        verifyUnit proxy = \case
            String u' | u' == T.pack u -> pure ()
            _ -> fail $
                "failed to parse quantified value. Expected value in '" <> u
                <> "' (e.g. { \"unit\": \"" <> u <> "\", \"quantity\": ... })"
                <> " but got something else."
          where
            u = symbolVal proxy

instance FromText b => FromText (Quantity sym b) where
    fromText = fmap Quantity . fromText

instance ToText b => ToText (Quantity sym b) where
    toText (Quantity b) = toText b

-- Builds (Quantity "lovelace" Word64) as "42 lovelace"
instance (KnownSymbol unit, Buildable a) => Buildable (Quantity unit a) where
    build (Quantity a) = build a <> fmt " " <> build u
      where
        u = symbolVal (Proxy :: Proxy unit)

{-------------------------------------------------------------------------------
                                Percentage
-------------------------------------------------------------------------------}

-- | Opaque Haskell type to represent values between 0 and 100 (incl).
newtype Percentage = Percentage
    { getPercentage :: Rational }
    deriving stock (Generic, Eq, Ord)
    deriving Show via (Quiet Percentage)

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
        $ (s / 100)

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
        left (const err) . mkPercentage $ (p / 100)
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

-- | Desired number of digits after the decimal point for presenting the
-- @Percentage@ type.
percentageNumberOfFractionalDigits :: Int
percentageNumberOfFractionalDigits = 2

-- | Round a @Rational@ to the given amount of fractional digits.
--
-- Note: This is safe to call on repeating digits, in contrast to @fromRational@
-- (for creating a @Scientific@).
rationalToToScientific :: Int -> Rational -> Scientific
rationalToToScientific fracDigits x = (conv i) / (conv factor)
  where
    i :: Int
    i = round (factor * x)

    conv :: (Real a, Fractional b) => a -> b
    conv = fromRational . toRational

    factor = 10 ^ fracDigits

-- | Turn a @Percentage@ to a @Double@ (without any extra rounding.)
percentageToDouble :: Percentage -> Double
percentageToDouble = fromRational . toRational . getPercentage
