{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.Types.WalletBalance where

import Prelude

import Cardano.Wallet.Api.JSON
    ( defaultRecordTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.Types.Amount as T

data WalletBalance = WalletBalance
    { _available :: !T.Amount
    , _total :: !T.Amount
    } deriving (Eq, Generic, Show)

instance FromJSON WalletBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletBalance where
    toJSON = genericToJSON defaultRecordTypeOptions
