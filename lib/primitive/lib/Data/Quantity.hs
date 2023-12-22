{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Representation of values with an associated (free) unit of measure. Useful to
-- disambiguate primitive types like 'Int' or 'String' which can be in different
-- bases depending on the context.

module Data.Quantity
    ( Quantity (..)
    , fromCoin
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin)
    )
import Control.DeepSeq
    ( NFData
    )
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
    ( Parser
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable
    )
import Data.IntCast
    ( IsIntSubType
    , intCast
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import Fmt
    ( Buildable (..)
    , fmt
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( KnownSymbol
    , Symbol
    , symbolVal
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Numeric.Natural
    ( Natural
    )
import Quiet
    ( Quiet (..)
    )

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
    deriving stock (Data, Generic, Eq, Ord)
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

-- | Constructs a 'Quantity` from a 'Coin'.
--
fromCoin
    :: (Integral i, IsIntSubType Natural i ~ 'True)
    => Coin
    -> Quantity "lovelace" i
fromCoin (Coin c) = Quantity (intCast c)
