{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Utilities for mapping data types to/from JSON.
--
module Cardano.Wallet.Deposit.HTTP.Types.JSON.Encoding
    ( Custom (..)
    , ViaText (..)
    , customOptions
    ) where

import Prelude

import Data.Aeson
    ( FromJSON (..)
    , GFromJSON
    , GToJSON'
    , Options (..)
    , ToJSON (..)
    , Value
    , Zero
    , camelTo2
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    , withText
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (toText)
    )
import GHC.Generics
    ( Generic
    , Rep
    )

{-----------------------------------------------------------------------------
    Generics
------------------------------------------------------------------------------}
newtype Custom a = Custom {unCustom :: a}

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Custom a)
  where
    parseJSON = fmap Custom . genericParseJSON customOptions

instance (Generic a, GToJSON' Value Zero (Rep a)) => ToJSON (Custom a)
  where
    toJSON = genericToJSON customOptions . unCustom

customOptions :: Options
customOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
    , omitNothingFields = True
    }

{-----------------------------------------------------------------------------
    Text
------------------------------------------------------------------------------}
newtype ViaText a = ViaText {unViaText :: a}

instance FromText a => FromJSON (ViaText a) where
    parseJSON = fmap ViaText . fromTextJSON ""
instance ToText a => ToJSON (ViaText a) where
    toJSON = toTextJSON . unViaText

eitherToParser :: Show s => Either s a -> Parser a
eitherToParser = either (fail . show) pure

toTextJSON :: ToText a => a -> Value
toTextJSON = toJSON . toText

fromTextJSON :: FromText a => String -> Value -> Parser a
fromTextJSON n = withText n (eitherToParser . fromText)
