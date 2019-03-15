{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletBalance where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleRecordOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.V2.Types.Amount as T

data WalletBalance = WalletBalance
    { _available :: !T.Amount
    , _total :: !T.Amount
    } deriving (Eq, Generic, Show)

instance FromJSON WalletBalance where
    parseJSON = genericParseJSON simpleRecordOptions
instance ToJSON WalletBalance where
    toJSON = genericToJSON simpleRecordOptions
