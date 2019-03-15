{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.Amount
    ( Amount (..)
    ) where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleRecordOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Api.V2.Types.CurrencyUnit as T

data Amount = Amount
    { _quantity :: Natural
    , _unit :: T.CurrencyUnit
    } deriving (Eq, Generic, Show)

instance FromJSON Amount where
    parseJSON = genericParseJSON simpleRecordOptions
instance ToJSON Amount where
    toJSON = genericToJSON simpleRecordOptions
