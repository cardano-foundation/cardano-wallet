{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletStateStatus where

import Cardano.Wallet.Api.V2.JSON
    ( simpleEnumOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )
import Prelude
    ( Show )

data WalletStateStatus
    = Ready
    | Restoring
    deriving (Generic, Show)

instance FromJSON WalletStateStatus where
    parseJSON = genericParseJSON simpleEnumOptions
instance ToJSON WalletStateStatus where
    toJSON = genericToJSON simpleEnumOptions
