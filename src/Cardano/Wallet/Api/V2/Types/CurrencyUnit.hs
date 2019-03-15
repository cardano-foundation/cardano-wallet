{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.CurrencyUnit
    ( CurrencyUnit (Lovelace)
    ) where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleEnumOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

data CurrencyUnit
    = Lovelace
    | Unknown
    deriving (Eq, Generic, Show)

instance FromJSON CurrencyUnit where
    parseJSON = genericParseJSON simpleEnumOptions
instance ToJSON CurrencyUnit where
    toJSON = genericToJSON simpleEnumOptions
