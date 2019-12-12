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
    , (.:)
    , (.=)
    )
import Data.Aeson.Types
    ( Parser )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Text.Read
    ( decimal )
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
--
-- >>> Aeson.encode $ Quantity @"lovelace" 14
-- {"unit":"lovelace","quantity":14}
newtype Quantity (unit :: Symbol) a = Quantity { getQuantity :: a }
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (Bounded, Enum)

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

{-------------------------------------------------------------------------------
                                Percentage
-------------------------------------------------------------------------------}

-- | Opaque Haskell type to represent values between 0 and 100 (incl).
newtype Percentage = Percentage
    { getPercentage :: Word }
    deriving stock (Generic, Show, Eq, Ord)
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

instance ToText Percentage where
    toText (Percentage p) = T.pack (show p) <> "%"

instance FromText Percentage where
    fromText txt = do
        (p, u) <- left (const err) $ decimal txt
        unless (u == "%") $ Left err
        left (const err) $ mkPercentage @Integer p
      where
        err = TextDecodingError
            "expected a value between 0 and 100 with a '%' suffix (e.g. '14%')"

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
