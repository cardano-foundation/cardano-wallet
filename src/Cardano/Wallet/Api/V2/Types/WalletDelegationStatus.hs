{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletDelegationStatus where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleEnumOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

data WalletDelegationStatus
    = Delegating
    | NotDelegating
    deriving (Eq, Generic, Show)

instance FromJSON WalletDelegationStatus where
    parseJSON = genericParseJSON simpleEnumOptions
instance ToJSON WalletDelegationStatus where
    toJSON = genericToJSON simpleEnumOptions
