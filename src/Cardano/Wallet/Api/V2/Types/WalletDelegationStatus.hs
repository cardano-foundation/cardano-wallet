{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletDelegationStatus where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( defaultSumTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

data WalletDelegationStatus
    = Delegating
    | NotDelegating
    deriving (Eq, Generic, Show)

instance FromJSON WalletDelegationStatus where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON WalletDelegationStatus where
    toJSON = genericToJSON defaultSumTypeOptions
