{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
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
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (String)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson.Types
    ( Parser )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )

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
newtype Quantity (unit :: Symbol) a = Quantity a
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (Bounded, Enum)

instance NFData a => NFData (Quantity unit a)

-- >>> Aeson.encode $ Quantity @"lovelace" 14
-- {"unit":"lovelace","quantity":14}
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


{-------------------------------------------------------------------------------
                                Percentage
-------------------------------------------------------------------------------}

-- | Opaque Haskell type to represent values between 0 and 100 (incl).
newtype Percentage = Percentage Word
    deriving stock (Generic, Show, Eq)
    deriving newtype (ToJSON)

instance NFData Percentage

instance FromJSON Percentage where
    parseJSON x = do
        n <- parseJSON x
        either (fail . show) return (mkPercentage @Int n)

instance Bounded Percentage where
    minBound = Percentage 0
    maxBound = Percentage 100

instance Enum Percentage where
    fromEnum (Percentage p) = fromEnum p
    toEnum = either (error . ("toEnum: " <>) . show) id . mkPercentage

-- | Safe constructor for 'Percentage'
mkPercentage
    :: Integral i
    => i
    -> Either MkPercentageError Percentage
mkPercentage i
    | let Percentage inf = minBound in i < fromIntegral inf =
        Left PercentageOutOfBoundsError
    | let Percentage sup = maxBound in i > fromIntegral sup =
        Left PercentageOutOfBoundsError
    | otherwise =
        pure $ Percentage $ fromIntegral i

data MkPercentageError
    = PercentageOutOfBoundsError
    deriving (Show, Eq)
