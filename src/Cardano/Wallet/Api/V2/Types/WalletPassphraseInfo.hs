{-# LANGUAGE DeriveGeneric #-}

module Cardano.Wallet.Api.V2.Types.WalletPassphraseInfo where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( defaultRecordTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import Data.Time.Clock
    ( UTCTime )
import GHC.Generics
    ( Generic )

newtype WalletPassphraseInfo = WalletPassphraseInfo
    { _lastUpdatedAt :: UTCTime
    } deriving (Eq, Generic, Show)

instance FromJSON WalletPassphraseInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletPassphraseInfo where
    toJSON = genericToJSON defaultRecordTypeOptions
