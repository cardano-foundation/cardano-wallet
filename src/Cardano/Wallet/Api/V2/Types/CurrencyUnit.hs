{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.CurrencyUnit
    ( CurrencyUnit (Lovelace)
    ) where

import Prelude

import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value (String) )
import GHC.Generics
    ( Generic )

data CurrencyUnit
    = Lovelace
    deriving (Eq, Generic, Show)

instance FromJSON CurrencyUnit where
    parseJSON (String "lovelace") = pure Lovelace
    parseJSON _ = fail "Invalid currency unit"
instance ToJSON CurrencyUnit where
    toJSON Lovelace = String "lovelace"
