{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.CurrencyUnit
    ( CurrencyUnit (..)
    ) where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( defaultSumTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

data CurrencyUnit
    = Lovelace
    deriving (Eq, Generic, Show)

instance FromJSON CurrencyUnit where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON CurrencyUnit where
    toJSON = genericToJSON defaultSumTypeOptions
