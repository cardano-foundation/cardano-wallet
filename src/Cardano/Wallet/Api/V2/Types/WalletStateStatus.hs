{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletStateStatus where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( defaultSumTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

data WalletStateStatus
    = Ready
    | Restoring
    deriving (Eq, Generic, Show)

instance FromJSON WalletStateStatus where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON WalletStateStatus where
    toJSON = genericToJSON defaultSumTypeOptions
