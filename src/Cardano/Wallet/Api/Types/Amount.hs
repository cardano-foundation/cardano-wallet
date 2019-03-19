{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.Types.Amount
    ( Amount (..)
    ) where

import Prelude

import Cardano.Wallet.Api.JSON
    ( defaultRecordTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Api.Types.CurrencyUnit as T

data Amount = Amount
    { _quantity :: Natural
    , _unit :: T.CurrencyUnit
    } deriving (Eq, Generic, Show)

instance FromJSON Amount where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Amount where
    toJSON = genericToJSON defaultRecordTypeOptions
